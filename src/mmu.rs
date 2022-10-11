// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! # Page tables and the MMU.
//!
//! We support paging for 64-bit operation in early boot, but
//! not anything close to approaching the generality of virtual
//! memory that would be supported in a full operating system.
//! While limiting in some respects, we do not need anything
//! more complex, and this allows us to make a number of
//! simplifying assumptions:
//!
//! * The loader virtual address space is identity mapped.  That
//!   is, every mapped loader address is in bijection with the
//!   corresponding physical addresses that map to them.
//! * Pages used to define the paging structures are allocated
//!   from the loader and mapped into its virtual address space.
//! * As a consequence of the above two points, every page in
//!   the page table radix tree is mapped in the loader at its
//!   physical address.  We may take any PTE in an inner node in
//!   the table, extract its physical address, and cast that to
//!   a valid pointer to a Table structure.
//! * While we support the creation of multiple address spaces
//!   (we need to remap the loader itself on entry to Rust code)
//!   we run on a single CPU in a single-threaded environment.
//!   Exactly one virtual address space is active at any given
//!   time globally across the machine.
//!
//! ## Notes on Types and Traits
//!
//! We endeavor to use the type system to statically prevent
//! common error categories.  For instance, we enforce at
//! compile time that pages of a particular type (4KiB, 2MiB,
//! 1GiB) are only mapped to physical page frames of the
//! corresponding type.
//!
//! Generally, we try to adhere to the, "Parse Don't Validate"
//! type-driven design philosophy.  Once we have parsed data
//! into a type that represents some invariant, we do not
//! continually recheck that invariant.  For example, we may
//! parse a virtual memory address into a, `Page2M` variable;
//! we then trust that the contained address is appropriately
//! aligned and canonical.
//!
//! The major traits involved in MMU handling are:
//!
//! * `Frame` --- Describes facets of physical page frames that
//!   are used when creating page table entries.  These include
//!   the frame's physical address and whether it is "big"
//!   (Large or Huge in x86 parlance).  Concrete frame types are
//!   defined for various sizes/alignments.
//! * `Page` --- Describes pages of virtual memory, including
//!   associating them with their corresponding physical frame
//!   type.  Concrete page types are defined for various
//!   sizes/alignments.
//! * `Table` --- A page table is really a hardware-defined
//!   radix tree.  The `Table` trait describes behaviors at a
//!   particular level in the tree.  Concrete types exist for
//!   tables at each tree level.
//! * `TableInner` --- An interior node in the tree can either,
//!   depending on its specific type, either map to a large page
//!   or point to a next-level page table.  This trait describes
//!   behaviors of table types that can point to other nodes.
//! * `Mapping` and the `Mapping(1|2|3|4)` enumerations ---
//!   These types tie `Page`/`Frame` pairs to `Table` types and
//!   are used when establishing mappings, as well as defining
//!   page attributes (readability, cacheability, etc).
//!
//! These details are hidden from the consumer, which interacts
//! with page tables via the `PageTable` type, which is a
//! wrapper around a `PML4` (the root of the paging radix tree).
//! This exposes the various kinds of mapping methods that can
//! make use of the above to enforce invariants.
//!
//! With these things in place, we can statically prevent many
//! paging errors; interior nodes in the tree always have the
//! correct permissions, frame and page sizes and alignment
//! always correspond, etc.  Regardless, there are some errors
//! we make no attempt to prevent: for instance, nothing
//! prevents us from mapping MMIO space onto the loader text
//! segment, or otherwise overwriting existing mappings, etc,
//! and so the `map` operation is unsafe.
//!
//! ## Physical Memory and Interaction with the Host OS
//!
//! The loader has a contract with the host operating system
//! that imposes some constraints on memory consumed by the MMU
//! code.  In particular, for the page table that we enter the
//! host operating system on, the host OS requires that,
//!
//! * All memory frames in the page table come from a physically
//!   contiguous region,
//! * That the root table (PML4) must be at the lowest physical
//!   address in that contiguous range,
//! * That the range must has no fewer 16 4KiB pages (but may
//!   have more).
//! * All non-page-table pages that map part of the host OS
//!   "kernel nucleus" image must set bit 11 in their PTEs.
//!
//! See RFD 215 for details.
//!
//! In order to maintain these properties, we define a special
//! memory allocator specific to page table allocation that
//! draws from a 4KiB aligned static buffer.

extern crate alloc;

use crate::mem;
use alloc::boxed::Box;
use alloc::vec::Vec;
use bitstruct::bitstruct;
use core::ops::Range;

const NULL: *const () = core::ptr::null();

/// We start with basic page and frame types.

/// Traits common to page frame numbers.  PFNs of different
/// sizes represent aligned frames of physical address space.
trait Frame {
    /// True if the frame is larger than 4KiB.
    const BIG: bool;
    const SIZE: usize;

    /// Returns a new Frame of the given type.
    fn new(addr: u64) -> Self;

    /// Returns the physical address of the frame.
    fn phys_addr(self) -> u64;
}

/// An aligned 4KiB span of physical address space.
/// XXX(cross): Provide an invariant enforcing implementation.
#[derive(Clone, Copy, Debug)]
#[repr(transparent)]
struct PFN4K(u64);
impl Frame for PFN4K {
    const BIG: bool = false;
    const SIZE: usize = 1 << 12;

    fn new(addr: u64) -> PFN4K {
        assert_eq!(addr % Self::SIZE as u64, 0);
        PFN4K(addr)
    }

    fn phys_addr(self) -> u64 {
        self.0
    }
}

/// An aligned 2MiB span of physical address space.
/// XXX(cross): Provide an invariant enforcing implementation.
#[derive(Clone, Copy, Debug)]
#[repr(transparent)]
struct PFN2M(u64);
impl Frame for PFN2M {
    const BIG: bool = true;
    const SIZE: usize = 1 << 21;

    fn new(addr: u64) -> PFN2M {
        assert_eq!(addr % Self::SIZE as u64, 0);
        PFN2M(addr)
    }

    fn phys_addr(self) -> u64 {
        self.0
    }
}

/// An aligned 1GiB span of physical address space.
/// XXX(cross): Provide an invariant enforcing implementation.
#[derive(Clone, Copy, Debug)]
#[repr(transparent)]
struct PFN1G(u64);
impl Frame for PFN1G {
    const BIG: bool = true;
    const SIZE: usize = 1 << 30;

    fn new(addr: u64) -> PFN1G {
        assert_eq!(addr % Self::SIZE as u64, 0);
        PFN1G(addr)
    }

    fn phys_addr(self) -> u64 {
        self.0
    }
}

/// Represents a 4KiB page of virtual memory, aligned on a 4KiB
/// boundary.
/// XXX(cross): Provide an invariant enforcing implementation.
#[derive(Clone)]
struct Page4K(usize);
impl Page4K {
    fn new(va: usize) -> Self {
        assert_eq!(va % <Self as Page>::FrameType::SIZE, 0);
        Self(va)
    }
}

/// Represents a 2MiB page of virtual memory, aligned on a 2MiB
/// boundary.
/// XXX(cross): Provide an invariant enforcing implementation.
struct Page2M(usize);
impl Page2M {
    fn new(va: usize) -> Self {
        assert_eq!(va % <Self as Page>::FrameType::SIZE, 0);
        Self(va)
    }
}

/// Represents a 1GiB page of virtual memory, aligned on a 1GiB
/// boundary.
/// XXX(cross): Provide an invariant enforcing implementation.
struct Page1G(usize);
impl Page1G {
    fn new(va: usize) -> Self {
        assert_eq!(va % <Self as Page>::FrameType::SIZE, 0);
        Self(va)
    }
}

/// Represents some mapping from a virtual page to a physical
/// frame of the corresponding type.
trait Mapping {
    fn virt_addr(&self) -> *const ();
}

/// Representable mappings at the PML1 level.
enum Mapping1 {
    Map4K(Page4K, PFN4K, mem::Attrs),
}

impl Mapping for Mapping1 {
    fn virt_addr(&self) -> *const () {
        match self {
            Mapping1::Map4K(page, _, _) => NULL.with_addr(page.addr()),
        }
    }
}

/// Mappings representable at the PML2 level.
enum Mapping2 {
    Map2M(Page2M, PFN2M, mem::Attrs),
    Next(Mapping1),
}

impl Mapping for Mapping2 {
    fn virt_addr(&self) -> *const () {
        match self {
            Mapping2::Map2M(page, _, _) => NULL.with_addr(page.addr()),
            Mapping2::Next(mapping1) => mapping1.virt_addr(),
        }
    }
}

/// Representable mappings at the PML3 level.
enum Mapping3 {
    Map1G(Page1G, PFN1G, mem::Attrs),
    Next(Mapping2),
}

impl Mapping for Mapping3 {
    fn virt_addr(&self) -> *const () {
        match self {
            Mapping3::Map1G(page, _, _) => NULL.with_addr(page.addr()),
            Mapping3::Next(mapping2) => mapping2.virt_addr(),
        }
    }
}

/// Representable mappings at the PML4 (root) level.
enum Mapping4 {
    Next(Mapping3),
}

impl Mapping for Mapping4 {
    fn virt_addr(&self) -> *const () {
        match self {
            Mapping4::Next(mapping3) => mapping3.virt_addr(),
        }
    }
}

/// Traits shared by pages of all types.
trait Page {
    /// The associated frame type for this page type.
    type FrameType: Frame;

    /// Creates a Mapping enumeration binding a typed page and
    /// frame for this type of page.
    fn mapping(
        page: Self,
        frame: Self::FrameType,
        attrs: mem::Attrs,
    ) -> Mapping4;

    // Returns the virtual address of the page.
    fn addr(&self) -> usize;
}

impl Page for Page4K {
    type FrameType = PFN4K;

    fn mapping(
        page: Self,
        frame: Self::FrameType,
        attrs: mem::Attrs,
    ) -> Mapping4 {
        let mapping = Mapping1::Map4K(page, frame, attrs);
        Mapping4::Next(Mapping3::Next(Mapping2::Next(mapping)))
    }

    fn addr(&self) -> usize {
        self.0
    }
}

impl Page for Page2M {
    type FrameType = PFN2M;

    fn mapping(
        page: Self,
        frame: Self::FrameType,
        attrs: mem::Attrs,
    ) -> Mapping4 {
        Mapping4::Next(Mapping3::Next(Mapping2::Map2M(page, frame, attrs)))
    }

    fn addr(&self) -> usize {
        self.0
    }
}

impl Page for Page1G {
    type FrameType = PFN1G;

    fn mapping(
        page: Self,
        frame: Self::FrameType,
        attrs: mem::Attrs,
    ) -> Mapping4 {
        Mapping4::Next(Mapping3::Map1G(page, frame, attrs))
    }

    fn addr(&self) -> usize {
        self.0
    }
}

bitstruct! {
    /// A basic page table entry used at any level of the paging
    /// hierarchy.  Note that the loader only uses a small subset
    /// of paging functionality, so we don't define every bit
    /// defined by the hardware.
    ///
    /// Bit 11 is special.  This is one of the architecturally
    /// "ignored" bits available for use by system software; we
    /// use it as part of the contract with the host operating
    /// system: setting it on a leaf marks a page containing
    /// part of the host OS kernel.
    ///
    /// We don't use the user bit, but the host OS expects it to
    /// be set on the interior paging structures, so we define
    /// it here.
    #[derive(Copy, Clone, Debug)]
    struct PTE(u64) {
        p: bool = 0;
        w: bool = 1;
        u: bool = 2;
        wt: bool = 3;
        nc: bool = 4;
        // a: bool = 5;
        // d: bool = 6;
        h: bool = 7;  // Large or Huge page.
        // g: bool = 8;
        //ign: u8 = 9..12;
        k: bool = 11;
        pfn: u64 = 12..51;
        nx: bool = 63;
    }
}

impl PTE {
    /// Returns an empty PTE.
    const fn empty() -> PTE {
        PTE(0)
    }

    const fn from_phys_addr(pa: u64) -> PTE {
        PTE(pa)
    }

    fn phys_addr(&self) -> u64 {
        self.pfn() << 12
    }

    /// Creates a new PTE for the given page frame number and
    /// permissions.
    fn new<F: Frame>(pa: F, attrs: mem::Attrs) -> PTE {
        PTE::from_phys_addr(pa.phys_addr())
            .with_p(attrs.r())
            .with_w(attrs.w())
            .with_nx(!attrs.x())
            .with_wt(!attrs.c())
            .with_nc(!attrs.c())
            .with_k(attrs.k())
            .with_h(F::BIG)
    }

    /// Creates a new PTE for a table at any level in the radix
    /// tree.
    fn new_for_table<T: Table>(table: &T) -> PTE {
        let ptr: *const T = table;
        // Note that tables are identity mapped.
        let pa = ptr.addr() as u64;
        PTE::from_phys_addr(pa)
            .with_p(true)
            .with_w(true)
            .with_wt(false)
            .with_nc(false)
            .with_nx(false)
            .with_u(true)
    }

    /// Returns the permissions of the given entry (if any).
    fn attrs(self) -> mem::Attrs {
        mem::Attrs::new(self.p(), self.w(), !self.nx(), !self.nc(), self.k())
    }

    /// Returns the virtual address of the table mapped by this address.
    ///
    /// # Safety
    /// Tables are taken from the identity mapped region of the
    /// address space.
    unsafe fn virt_addr(self) -> *const () {
        const NIL: *const () = core::ptr::null();
        NIL.with_addr(self.phys_addr() as usize)
    }
}

#[cfg(test)]
mod pte_tests {
    use super::{Frame, PFN2M, PFN4K, PTE};
    use crate::mem;

    #[test]
    fn simple() {
        let pte = PTE::from_phys_addr(0xF00F_F000)
            .with_p(true)
            .with_w(true)
            .with_u(true);
        assert_eq!(pte.0, 0xF00F_F007);
        assert_eq!(pte.pfn(), 0xF_00FF);
        assert!(!pte.nc());
        assert!(!pte.nx());
    }

    #[test]
    fn nx() {
        let pte = PTE(0).with_pfn(0xF_00FF).with_nx(true);
        assert_eq!(pte.0, 0x8000_0000_F00F_F000);
    }

    #[test]
    fn constructed() {
        let frame = PFN4K::new(0xF00D_F000);
        let attrs = mem::Attrs::new(true, true, true, true, true);
        let pte = PTE::new(frame, attrs);
        assert_eq!(pte.0, 0b1111_0000_0000_1101_1111_1000_0000_0011);
    }

    #[test]
    fn constructed_large() {
        let frame = PFN2M::new(0xF000_0000);
        let attrs = mem::Attrs::new(true, true, false, false, true);
        let pte = PTE::new(frame, attrs);
        const NX: u64 = 1 << 63;
        assert_eq!(pte.0, NX | 0b1111_0000_0000_0000_0000_1000_1001_1011);
    }
}

/// Traits shared by tables at all levels in the paging radix
/// tree.
trait Table: Sized {
    /// The associated entry type mapped by this table type.
    type EntryType;

    /// The mapping type supported by this level of the tree.
    type MappingType: Mapping;

    /// The number of bits required to shift a virtual address
    /// to find its index in a table of this type.
    const INDEX_SHIFT: usize;

    /// Creates a new table of the current type.  This is
    /// allocated from the special paging-specific table
    /// allocator.
    fn new() -> &'static mut Self {
        let table = Box::<Self, _>::new_zeroed_in(TableAlloc);
        Box::leak(unsafe { table.assume_init() })
    }

    /// Returns an entry in the current table for the given
    /// virtual address.
    fn entry(&mut self, va: *const ()) -> Option<Self::EntryType>;

    /// Sets the entry corresponding to the given virtual
    /// address.
    ///
    /// # Safety
    /// The caller must ensure that the given entry type and
    /// permissions are appropriate for the virtual address
    /// space.  This method does not ensure that one does not
    /// overwrite part of the loader, or map a cached-page onto
    /// MMIO space, for example.
    unsafe fn set_entry(
        &mut self,
        va: *const (),
        entry: Option<Self::EntryType>,
    );

    /// Establishes a mapping of the appropriate type for this
    /// level of the tree in the table.
    ///
    /// # Safety
    /// The caller must ensure that the given mapping is
    /// appropriate for the virtual address space.  This method
    /// will overwrite any existing mappings.  Be sure not to
    /// overwrite the loader or inappropriately map MMIO space.
    unsafe fn map(&mut self, mapping: Self::MappingType);

    /// Computes the table entry index for the given virtual
    /// address in the current table.
    fn index(va: *const ()) -> usize {
        (va.addr() >> Self::INDEX_SHIFT) & 0x1FF
    }
}

/// Interior table types in the radix tree implement this trait
/// to establish behaviors specific to nodes that can point to
/// other nodes.
trait InnerTable: Table {
    /// The type of table at the next lower level in the paging
    /// radix tree.
    type NextTableType: Table;

    /// Returns a mutable reference to the next-level page table
    /// for the given virtual address, or None if no such table
    /// exists.
    fn next_mut(
        &mut self,
        va: *const (),
    ) -> Option<&'static mut Self::NextTableType>;
}

/// A PML4 is the highest level of the paging radix tree.
#[repr(C, align(4096))]
struct PML4 {
    entries: [PTE; 512],
}

/// The only valid entries in the PML4 are pointers to PML3s.
enum PML4E {
    Next(&'static mut PML3),
}

impl InnerTable for PML4 {
    type NextTableType = PML3;

    fn next_mut(&mut self, va: *const ()) -> Option<&'static mut PML3> {
        let entry = self.entries[Self::index(va)];
        entry.p().then(|| {
            let p = unsafe { entry.virt_addr() };
            assert!(
                !p.is_null() && p.is_aligned_to(core::mem::align_of::<PML3>())
            );
            unsafe { &mut *TableAlloc::try_with_addr(p.addr()).unwrap() }
        })
    }
}

impl Table for PML4 {
    type EntryType = PML4E;
    type MappingType = Mapping4;
    const INDEX_SHIFT: usize = 39;

    fn entry(&mut self, va: *const ()) -> Option<Self::EntryType> {
        self.next_mut(va).map(PML4E::Next)
    }

    unsafe fn set_entry(&mut self, va: *const (), entry: Option<PML4E>) {
        self.entries[PML4::index(va)] = match entry {
            None => PTE::empty(),
            Some(PML4E::Next(table)) => PTE::new_for_table(table),
        };
    }

    unsafe fn map(&mut self, mapping: Mapping4) {
        let va = mapping.virt_addr();
        if self.entry(va).is_none() {
            unsafe {
                self.set_entry(va, Some(PML4E::Next(PML3::new())));
            }
        }
        if let Some(table) = self.next_mut(va) {
            let Mapping4::Next(mapping3) = mapping;
            unsafe {
                table.map(mapping3);
            }
        }
    }
}

/// The PML3 is the second highest level in the paging radix
/// tree.  It can either map 1GiB "huge" pages.
#[repr(C, align(4096))]
struct PML3 {
    entries: [PTE; 512],
}

/// PML3 entries either point to a 1GiB page frame, or to a
/// PML2.
enum PML3E {
    Next(&'static mut PML2),
    Page(PFN1G, mem::Attrs),
}

impl InnerTable for PML3 {
    type NextTableType = PML2;

    fn next_mut(&mut self, va: *const ()) -> Option<&'static mut PML2> {
        let entry = self.entries[Self::index(va)];
        (entry.p() && !entry.h()).then(|| {
            let p = unsafe { entry.virt_addr() };
            assert!(
                !p.is_null() && p.is_aligned_to(core::mem::align_of::<PML2>())
            );
            unsafe { &mut *TableAlloc::try_with_addr(p.addr()).unwrap() }
        })
    }
}

impl Table for PML3 {
    type EntryType = PML3E;
    type MappingType = Mapping3;
    const INDEX_SHIFT: usize = 30;

    fn entry(&mut self, va: *const ()) -> Option<Self::EntryType> {
        let entry = self.entries[Self::index(va)];
        match (entry.p(), entry.h()) {
            (false, _) => None,
            (_, false) => self.next_mut(va).map(PML3E::Next),
            (_, true) => {
                Some(PML3E::Page(PFN1G::new(entry.phys_addr()), entry.attrs()))
            }
        }
    }

    unsafe fn set_entry(&mut self, va: *const (), entry: Option<PML3E>) {
        self.entries[PML3::index(va)] = match entry {
            None => PTE::empty(),
            Some(PML3E::Next(table)) => PTE::new_for_table(table),
            Some(PML3E::Page(page, attrs)) => PTE::new(page, attrs),
        };
    }

    unsafe fn map(&mut self, mapping: Mapping3) {
        let va = mapping.virt_addr();
        match mapping {
            Mapping3::Map1G(_, frame, attrs) => unsafe {
                self.set_entry(va, Some(PML3E::Page(frame, attrs)));
            },
            Mapping3::Next(mapping2) => {
                if self.entry(va).is_none() {
                    unsafe {
                        self.set_entry(va, Some(PML3E::Next(PML2::new())));
                    }
                }
                if let Some(table) = self.next_mut(va) {
                    unsafe {
                        table.map(mapping2);
                    }
                }
            }
        }
    }
}

/// The PML2 is the third-highest type of table in the paging
/// tree.
#[repr(C, align(4096))]
struct PML2 {
    entries: [PTE; 512],
}

/// PML2 entries can either point to a PML1, or to a 2MiB
/// "large" page.
enum PML2E {
    Next(&'static mut PML1),
    Page(PFN2M, mem::Attrs),
}

impl InnerTable for PML2 {
    type NextTableType = PML1;

    fn next_mut(&mut self, va: *const ()) -> Option<&'static mut PML1> {
        let entry = self.entries[Self::index(va)];
        (entry.p() && !entry.h()).then(|| {
            let p = unsafe { entry.virt_addr() };
            assert!(
                !p.is_null() && p.is_aligned_to(core::mem::align_of::<PML1>())
            );
            unsafe { &mut *TableAlloc::try_with_addr(p.addr()).unwrap() }
        })
    }
}

impl Table for PML2 {
    type EntryType = PML2E;
    type MappingType = Mapping2;
    const INDEX_SHIFT: usize = 21;

    fn entry(&mut self, va: *const ()) -> Option<Self::EntryType> {
        let entry = self.entries[Self::index(va)];
        match (entry.p(), entry.h()) {
            (false, _) => None,
            (_, false) => self.next_mut(va).map(PML2E::Next),
            (_, true) => {
                Some(PML2E::Page(PFN2M::new(entry.phys_addr()), entry.attrs()))
            }
        }
    }

    unsafe fn set_entry(&mut self, va: *const (), entry: Option<PML2E>) {
        self.entries[PML2::index(va)] = match entry {
            None => PTE::empty(),
            Some(PML2E::Next(table)) => PTE::new_for_table(table),
            Some(PML2E::Page(page, attrs)) => PTE::new(page, attrs),
        }
    }

    unsafe fn map(&mut self, mapping: Mapping2) {
        let va = mapping.virt_addr();
        match mapping {
            Mapping2::Map2M(_, frame, attrs) => unsafe {
                self.set_entry(va, Some(PML2E::Page(frame, attrs)));
            },
            Mapping2::Next(mapping1) => {
                if self.entry(va).is_none() {
                    unsafe {
                        self.set_entry(va, Some(PML2E::Next(PML1::new())));
                    }
                }
                if let Some(table) = self.next_mut(va) {
                    unsafe {
                        table.map(mapping1);
                    }
                }
            }
        }
    }
}

/// The PML1 represents a terminal leaf note in the paging radix
/// tree.
#[repr(C, align(4096))]
struct PML1 {
    entries: [PTE; 512],
}

/// Valid PML1 entries can only point to 4KiB page frames.
enum PML1E {
    Page(PFN4K, mem::Attrs),
}

impl Table for PML1 {
    type EntryType = PML1E;
    type MappingType = Mapping1;
    const INDEX_SHIFT: usize = 12;

    fn entry(&mut self, va: *const ()) -> Option<Self::EntryType> {
        let entry = self.entries[Self::index(va)];
        entry
            .p()
            .then(|| PML1E::Page(PFN4K::new(entry.phys_addr()), entry.attrs()))
    }

    unsafe fn set_entry(&mut self, va: *const (), entry: Option<PML1E>) {
        self.entries[PML1::index(va)] = match entry {
            None => PTE::empty(),
            Some(PML1E::Page(page, attrs)) => PTE::new(page, attrs),
        };
    }

    unsafe fn map(&mut self, mapping: Mapping1) {
        let Mapping1::Map4K(_, frame, attrs) = mapping;
        unsafe {
            self.set_entry(
                mapping.virt_addr(),
                Some(PML1E::Page(frame, attrs)),
            );
        }
    }
}

/// Represents a complete page table.
#[repr(C, align(4096))]
pub(crate) struct PageTable {
    pml4: PML4,
}

impl PageTable {
    /// Creates a new static page table, zero it, and returns
    /// a reference to it.
    pub(crate) fn new() -> &'static mut PageTable {
        let table = Box::<Self, _>::new_zeroed_in(TableAlloc);
        Box::leak(unsafe { table.assume_init() })
    }

    /// Loads the page table into the MMU.
    pub(crate) unsafe fn activate(&'static mut self) -> &'static mut PageTable {
        let pa = self.phys_addr();
        unsafe {
            core::arch::asm!("movq {pa}, %cr3", pa = in(reg) pa, options(att_syntax));
        }
        self
    }

    /// Returns the physical address of the root of the page
    /// table radix tree.
    pub(crate) fn phys_addr(&self) -> u64 {
        let ptr: *const PML4 = &self.pml4;
        // Note that the PML4 is identity mapped.
        ptr.addr() as u64
    }

    /// Identity maps an address space.
    pub(crate) unsafe fn identity_map(&mut self, regions: &[mem::Region]) {
        for region in regions {
            let pa = mem::P4KA::new(region.start().addr() as u64);
            unsafe {
                self.map_region(region, pa);
            }
        }
    }

    /// Maps a single region of virtual address space to some
    /// region of contiguous physical address space.  Permits
    /// mapping at the end of the address range.
    unsafe fn map_region(&mut self, region: &mem::Region, pa: mem::P4KA) {
        let mut start = region.start().addr();
        let end = region.end().addr();
        assert!(mem::is_canonical_range(start, end));
        let mut pa = pa.phys_addr();
        assert!(mem::is_physical(
            pa.checked_add(end.wrapping_sub(start) as u64).unwrap()
        ));
        while start != end {
            let attrs = region.attrs();
            let len = if end.wrapping_sub(start) >= PFN1G::SIZE
                && start % PFN1G::SIZE == 0
                && (pa as usize) % PFN1G::SIZE == 0
            {
                unsafe {
                    self.map(Page1G::new(start), PFN1G::new(pa as u64), attrs);
                }
                PFN1G::SIZE
            } else if end.wrapping_sub(start) >= PFN2M::SIZE
                && start % PFN2M::SIZE == 0
                && (pa as usize) % PFN2M::SIZE == 0
            {
                unsafe {
                    self.map(Page2M::new(start), PFN2M::new(pa as u64), attrs);
                }
                PFN2M::SIZE
            } else if end.wrapping_sub(start) >= PFN4K::SIZE
                && start % PFN4K::SIZE == 0
                && (pa as usize) % PFN4K::SIZE == 0
            {
                unsafe {
                    self.map(Page4K::new(start), PFN4K::new(pa as u64), attrs);
                }
                PFN4K::SIZE
            } else {
                panic!("bad page size");
            };
            start = start.wrapping_add(len);
            pa = pa.checked_add(len as u64).unwrap();
        }
    }

    /// Map a page of some size and alignment to the
    /// corresponding frame type, with the given attributes.
    unsafe fn map<P: Page>(
        &mut self,
        page: P,
        frame: P::FrameType,
        attrs: mem::Attrs,
    ) {
        unsafe {
            self.pml4.map(P::mapping(page, frame, attrs));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // This test is unfortunately long, but we construct an
    // entire address space and then probe it completely.
    #[test]
    fn test_an_addr_space() {
        let regions = &[
            mem::Region::new(
                mem::V4KA::new(0x1000_0000)..mem::V4KA::new(0x1000_1000),
                mem::Attrs::new_text(),
            ),
            mem::Region::new(
                mem::V4KA::new(0x1000_2000)..mem::V4KA::new(0x1000_4000),
                mem::Attrs::new_rodata(),
            ),
            mem::Region::new(
                mem::V4KA::new(0x1000_F000)..mem::V4KA::new(0x1200_0000),
                mem::Attrs::new_data(),
            ),
            mem::Region::new(
                mem::V4KA::new(0x1200_0000)..mem::V4KA::new(0x4000_0000),
                mem::Attrs::new_bss(),
            ),
            mem::Region::new(
                mem::V4KA::new(0x8000_0000)..mem::V4KA::new(0x1_0000_0000),
                mem::Attrs::new_mmio(),
            ),
        ];
        let page_table = PageTable::new();
        unsafe {
            page_table.identity_map(regions);
        }

        // Examine the PML4 entries.
        let pml4 = &mut page_table.pml4;

        let pml4es = pml4
            .entries
            .iter()
            .enumerate()
            .filter(|&(_, e)| e.p())
            .collect::<Vec<_>>();
        assert_eq!(pml4es.len(), 1);
        let (index, &entry) = pml4es[0];
        assert_eq!(index, 0);
        assert!(entry.p());
        assert!(entry.w());
        assert!(!entry.nx());
        assert!(!entry.nc());
        assert!(entry.u());

        // Examine the PML3 entries.  There should be a single
        // entry pointing to a PML2 for the loader, and two huge
        // pages for MMIO space.
        let pml3 = pml4.next_mut(NULL.with_addr(0x8000_0000)).unwrap();
        let n = pml3.entries.iter().filter(|&e| e.p()).count();
        assert_eq!(n, 3);
        let l0g = pml3.entries[0];
        assert!(l0g.p());
        assert!(l0g.w());
        assert!(!l0g.nc());
        assert!(!l0g.nx());
        assert!(l0g.u());
        let g1 = pml3.entries[1];
        assert!(!g1.p());
        let mmio2g = pml3.entries[2];
        assert!(mmio2g.p());
        assert!(mmio2g.w());
        assert!(mmio2g.h());
        assert!(mmio2g.nc());
        assert!(mmio2g.nx());
        assert!(!mmio2g.k());
        assert!(!mmio2g.u());
        assert_eq!(mmio2g.phys_addr(), 0x8000_0000);
        let mmio3g = pml3.entries[3];
        assert!(mmio3g.p());
        assert!(mmio3g.w());
        assert!(mmio3g.h());
        assert!(mmio3g.nc());
        assert!(mmio3g.nx());
        assert!(!mmio3g.k());
        assert!(!mmio3g.u());
        assert_eq!(mmio3g.phys_addr(), 0xC000_0000);

        // Check the PML2 entries.  The PML2 maps a gigabyte of
        // address space from 0 to 0x4000_0000.
        let pml2 = pml3.next_mut(NULL.with_addr(0x1000_0000)).unwrap();
        let n = pml2.entries.iter().filter(|&e| e.p()).count();
        assert_eq!(n, 512 - 512 / 4);
        // The lower quarter of the PML2 should be empty.
        for e in &pml2.entries[..128] {
            assert!(!e.p());
        }
        // Where the 2MiB entries start.
        let start2m = 0x1020_0000 >> 21;
        assert_eq!(start2m, 129);
        // There should be one PML1 mapping kernel text, rodata
        // and the first part of kernel data.
        for (k, e) in pml2.entries[128..start2m].iter().enumerate() {
            assert!(e.p());
            assert!(e.w());
            assert!(!e.nx(), "!e.nx() at {k}");
            assert!(!e.nc());
            assert!(e.u());
            assert!(!e.k());
        }
        // Check the 2MiB entries covering K data and K BSS.
        assert_eq!(0x40000000 >> 21, 512);
        for (k, e) in pml2.entries[start2m..].iter().enumerate() {
            assert!(e.p());
            assert!(e.w());
            assert!(e.h());
            assert!(e.nx());
            assert!(!e.nc());
            assert!(!e.k());
            assert!(!e.u());
            let expected_addr = start2m * (1 << 21) + k * (1 << 21);
            assert_eq!(e.phys_addr(), expected_addr as u64);
        }
        // Check the 4KiB PML1 entries.  There should be one
        // text page, two RO data pages, and a bunch of RW
        // data pages.
        let pml1 = pml2.next_mut(NULL.with_addr(0x1000_0000)).unwrap();
        // Text.
        assert!(pml1.entries[0].p());
        assert!(!pml1.entries[0].w());
        assert!(!pml1.entries[0].nx());
        assert!(!pml1.entries[0].h());
        assert!(!pml1.entries[0].u());
        assert!(!pml1.entries[0].nc());
        assert!(!pml1.entries[0].k());
        assert_eq!(pml1.entries[0].phys_addr(), 0x1000_0000);
        // Empty.
        assert!(!pml1.entries[1].p());
        // RO data.
        for (k, e) in pml1.entries[2..4].iter().enumerate() {
            assert!(e.p());
            assert!(!e.w());
            assert!(e.nx());
            assert!(!e.h());
            assert!(!e.u());
            assert!(!e.nc());
            assert!(!e.k());
            let offset = k as u64 * 4096;
            assert_eq!(e.phys_addr(), 0x1000_2000 + offset);
        }
        // A few more empty entries for 0x1000_4000..0x1000_F000
        for e in &pml1.entries[4..15] {
            assert!(!e.p());
        }
        // And finally, we should map 4KiB R/W data pages for
        // 0x1000_F000..0x1020_0000.
        for (k, e) in pml1.entries[15..].iter().enumerate() {
            assert!(e.p());
            assert!(e.w());
            assert!(e.nx());
            assert!(!e.h());
            assert!(!e.u());
            assert!(!e.nc());
            assert!(!e.k());
            let offset = k as u64 * 4096;
            assert_eq!(e.phys_addr(), 0x1000_F000 + offset);
        }
    }
}

/// A LoaderPageTable is a newtype around a PageTable that
/// prohibits some types of mappings.  In particular, it
/// maintains a list of regions that the consumer cannot
/// creating mappings in.
pub(crate) struct LoaderPageTable {
    page_table: &'static mut PageTable,
    reserved: Vec<Range<mem::V4KA>>,
}

impl LoaderPageTable {
    /// Creates a new LoaderPageTable from the given PageTable.
    pub(crate) fn new(
        page_table: &'static mut PageTable,
        reserved: &[Range<mem::V4KA>],
    ) -> LoaderPageTable {
        LoaderPageTable { page_table, reserved: reserved.into() }
    }

    /// Maps the given virtual region to the given physical
    /// address with the given attributes.
    pub(crate) unsafe fn map_region(
        &mut self,
        range: Range<mem::V4KA>,
        attrs: mem::Attrs,
        pa: mem::P4KA,
    ) -> crate::Result<()> {
        if Self::overlaps(&self.reserved, &range) {
            return Err("range overlaps reserved regions");
        }
        let len = range.end.addr().wrapping_sub(range.start.addr());
        let phys_addr = pa.phys_addr() as usize;
        let pstart = mem::V4KA::new(phys_addr);
        let pend = mem::V4KA::new(phys_addr.wrapping_add(len));
        let prange = pstart..pend;
        if Self::overlaps(&self.reserved, &prange) {
            return Err("physical range overlaps reserved regions");
        }
        let region = mem::Region::new(range, attrs);
        unsafe {
            self.page_table.map_region(&region, pa);
        }
        Ok(())
    }

    /// Returns true iff region `a` overlaps any of the regions
    /// in `rs`.
    ///
    /// Two regions `a` and `b` overlap iff `a` contains `b`'s
    /// start or `b` contains `a`'s start.  Note, however, that
    /// because address ranges in the loader are half-open and
    /// can wrap around the address space to (exactly) 0, we
    /// first convert the ranges to closed, inclusive ranges.
    fn overlaps(rs: &[Range<mem::V4KA>], a: &Range<mem::V4KA>) -> bool {
        let aa = a.start.addr()..=(a.end.addr().wrapping_sub(1));
        rs.iter().any(|range| {
            let rr = range.start.addr()..=(range.end.addr().wrapping_sub(1));
            rr.contains(aa.start()) || aa.contains(rr.start())
        })
    }

    /// Returns the physical address of the page table root.
    pub(crate) fn phys_addr(&self) -> u64 {
        self.page_table.phys_addr()
    }
}

#[cfg(test)]
mod loader_page_table_tests {
    use super::*;

    #[test]
    fn map_non_overlapping_reserved() {
        let page_table = PageTable::new();
        let reserved = &[mem::V4KA::new(0x1000)..mem::V4KA::new(0x8000)];
        let mut loader_page_table = LoaderPageTable::new(page_table, reserved);
        let region = mem::V4KA::new(0x8000)..mem::V4KA::new(0xa000);
        assert!(unsafe {
            loader_page_table
                .map_region(
                    region,
                    mem::Attrs::new_text(),
                    mem::P4KA::new(0x8000),
                )
                .is_ok()
        });
    }

    #[test]
    fn map_overlapping_reserved_fail() {
        let page_table = PageTable::new();
        let reserved = &[mem::V4KA::new(0x1000)..mem::V4KA::new(0x8000)];
        let mut loader_page_table = LoaderPageTable::new(page_table, reserved);
        let overlapping = mem::V4KA::new(0x6000)..mem::V4KA::new(0x9000);
        assert!(unsafe {
            loader_page_table
                .map_region(
                    overlapping,
                    mem::Attrs::new_rodata(),
                    mem::P4KA::new(0x2000),
                )
                .is_err()
        });
    }
}

mod arena {
    extern crate alloc;

    use super::Table;
    use crate::allocator::BumpAlloc;
    use alloc::alloc::{AllocError, Allocator, Layout};
    use core::ptr;
    use static_assertions::const_assert;

    const PAGE_SIZE: usize = 4096;
    const PAGE_ARENA_SIZE: usize = 128 * PAGE_SIZE;
    // This is trivially true, but keep the assert as
    // documentation of the minimum arena size invariant.
    // See RFD215 for details.
    const_assert!(PAGE_ARENA_SIZE > 16 * PAGE_SIZE);

    /// An allocator specialized for MMU page allocations.
    ///
    /// # Safety
    /// This is visibility restricted to this module, and
    /// the only allocations we take from it are PAGE_SIZE
    /// size and aligned.
    pub(super) struct TableAlloc;

    #[derive(Clone, Copy, Debug)]
    pub(super) enum Error {
        BadPointer,
    }

    impl TableAlloc {
        /// Try and convert an integer to a pointer.
        pub(super) fn try_with_addr<T: Table>(
            addr: usize,
        ) -> Result<*mut T, Error> {
            let base = unsafe { PAGE_ALLOCATOR.base() };
            let range = unsafe { PAGE_ALLOCATOR.addr_range() };
            if !range.contains(&addr) {
                return Err(Error::BadPointer);
            }
            let ptr = base.with_addr(addr);
            if !ptr.is_aligned_to(core::mem::align_of::<T>()) {
                return Err(Error::BadPointer);
            }
            Ok(ptr as *mut T)
        }
    }

    #[repr(C, align(4096))]
    struct PageArena([u8; PAGE_ARENA_SIZE]);
    static mut PAGES: PageArena = PageArena([0; PAGE_ARENA_SIZE]);
    static mut PAGE_ALLOCATOR: BumpAlloc<{ PAGE_ARENA_SIZE }> =
        BumpAlloc::new(unsafe { PAGES.0 });

    unsafe impl Allocator for TableAlloc {
        fn allocate(
            &self,
            layout: Layout,
        ) -> Result<ptr::NonNull<[u8]>, AllocError> {
            let align = layout.align();
            let size = layout.size();
            assert_eq!(align, PAGE_SIZE);
            assert_eq!(size, PAGE_SIZE);
            let a = unsafe { PAGE_ALLOCATOR.alloc_bytes(align, size) };
            let p = a.ok_or(AllocError)?;
            Ok(p.into())
        }
        unsafe fn deallocate(&self, _ptr: ptr::NonNull<u8>, _layout: Layout) {}
    }
}

use arena::TableAlloc;

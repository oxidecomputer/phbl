// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use core::ops::Range;

pub(crate) const KIB: usize = 1024;
pub(crate) const MIB: usize = 1024 * KIB;
pub(crate) const GIB: usize = 1024 * MIB;

/// A V4KA represents a 4KiB aligned, canonical virtual memory
/// address.  The address may or may not be mapped.
#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd)]
pub(crate) struct V4KA(usize);

/// Lower canonical address space supremum.
pub const LOW_CANON_SUP: usize = 0x0000_7FFF_FFFF_FFFF + 1;
// Higher canonical address space infimum.
pub const HI_CANON_INF: usize = 0xFFFF_8000_0000_0000 - 1;

/// Returns true IFF the given address is canonical.
const fn is_canonical(va: usize) -> bool {
    va <= 0x0000_7FFF_FFFF_FFFF || 0xFFFF_8000_0000_0000 <= va
}

/// Returns true IFF the address is a valid physical address.
pub const fn is_physical(pa: u64) -> bool {
    pa < (1 << 46)
}

/// Returns true IFF the range of virtual addresses
/// in [start, end) is canonical.
pub const fn is_canonical_range(start: usize, end: usize) -> bool {
    // If the range ends before it starts and end is not exactly
    // zero, the range is not canonical.
    if end < start && end != 0 {
        return false;
    }
    // If in the lower portion of the canonical address space,
    // end is permitted to be exactly one beyond the supremum.
    if start < LOW_CANON_SUP && end <= LOW_CANON_SUP {
        return true;
    }
    // Otherwise, the range is valid IFF it is in the upper
    // portion of the canonical address space, or end is 0.
    HI_CANON_INF < start && (HI_CANON_INF < end || end == 0)
}

impl V4KA {
    /// The alignment factor.
    pub(crate) const ALIGN: usize = 4096;
    pub(crate) const MASK: usize = Self::ALIGN - 1;
    pub(crate) const SIZE: usize = Self::ALIGN;

    /// Returns a new V4KA constructed from the given virtual
    /// address, which must be both canonical and properly
    /// aligned.
    pub(crate) const fn new(va: usize) -> V4KA {
        assert!(is_canonical(va));
        assert!(va & Self::MASK == 0);
        V4KA(va)
    }

    /// Returns the integer value of the raw virtual address.
    pub(crate) const fn addr(self) -> usize {
        self.0
    }
}

/// A P4KA represents a 4KiB aligned, valid address in the
/// physical address space.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct P4KA(u64);

impl P4KA {
    /// The alignment factor.
    pub(crate) const ALIGN: u64 = 4096;
    pub(crate) const MASK: u64 = Self::ALIGN - 1;

    /// Constructs a new P4KA from the given physical address,
    /// must be properly aligned and lie within the range of the
    /// physical address space.
    pub(crate) const fn new(pa: u64) -> P4KA {
        assert!(is_physical(pa));
        assert!(pa & Self::MASK == 0);
        P4KA(pa)
    }

    /// Returns the integer value of the raw physical address.
    pub(crate) const fn phys_addr(self) -> u64 {
        self.0
    }
}

/// Records the permissions of a mapped into the virtual address
/// space.
#[derive(Clone, Copy, Debug)]
pub(crate) struct Attrs {
    /// True if readable.
    r: bool,
    /// True if writable.
    w: bool,
    /// True if executable.
    x: bool,
    /// True if cacheable.
    c: bool,
    /// True if part of the kernel nucleus
    k: bool,
}

impl Attrs {
    /// Returns a new Attrs structure with the given permissions.
    pub(crate) fn new(r: bool, w: bool, x: bool, c: bool, k: bool) -> Attrs {
        Attrs { r, w, x, c, k }
    }

    /// Returns new attributes suitable for the loader.
    fn new_loader(r: bool, w: bool, x: bool) -> Attrs {
        Self::new(r, w, x, true, false)
    }

    /// Returns a new Attrs specialized for loader text.
    pub(crate) fn new_text() -> Attrs {
        Self::new_loader(true, false, true)
    }

    /// Returns a new Attrs specialized for loader read-only
    /// data.
    pub(crate) fn new_rodata() -> Attrs {
        Self::new_loader(true, false, false)
    }

    /// Returns a new Attrs specialized for loader read/write
    /// data.
    pub(crate) fn new_data() -> Attrs {
        Self::new_loader(true, true, false)
    }

    /// Returns new Attrs specialized for loader BSS.  These are
    /// functionally identical to data attributes.
    pub(crate) fn new_bss() -> Attrs {
        Self::new_data()
    }

    /// Returns new Attrs specialized for MMIO regions. Notably,
    /// these are uncached.
    pub(crate) fn new_mmio() -> Attrs {
        Self::new(true, true, false, false, false)
    }

    /// Returns new Attrs suitable for the host kernel nucleus.
    pub(crate) fn new_kernel(r: bool, w: bool, x: bool) -> Attrs {
        Self::new(r, w, x, true, true)
    }

    /// Returns true IFF readable.
    pub(crate) fn r(&self) -> bool {
        self.r
    }

    /// Returns true IFF writeable.
    pub(crate) fn w(&self) -> bool {
        self.w
    }

    /// Returns true IFF executable.
    pub(crate) fn x(&self) -> bool {
        self.x
    }

    /// Returns true IFF cacheable.
    pub(crate) fn c(&self) -> bool {
        self.c
    }

    /// Returns true IFF part of the host kernel.
    pub(crate) fn k(&self) -> bool {
        self.k
    }
}

/// A region of virtual memory.
#[derive(Clone, Debug)]
pub(crate) struct Region {
    range: Range<V4KA>,
    attrs: Attrs,
}

impl Region {
    /// Returns a new region spanning the given [start, end)
    /// address pair and attributes.
    pub fn new(range: Range<V4KA>, attrs: Attrs) -> Region {
        Region { range, attrs }
    }

    /// Returns the range start address.
    pub fn start(&self) -> V4KA {
        self.range.start
    }

    /// Returns the range end address.
    pub fn end(&self) -> V4KA {
        self.range.end
    }

    /// Returns the range attributes.
    pub fn attrs(&self) -> Attrs {
        self.attrs
    }
}

//! Initialize the loader environment.
//!
//! When this code is called, we are in the minimal state
//! established by the assembler code invoked from the reset
//! vector.  We know that:
//!
//! 1. We are in 64-bit long mode.
//! 2. The entire loader is covered by some virtual identity
//!    mapping, and is rwx and cached.
//! 3. UART MMIO space is mapped rw- and uncached.
//! 4. The BSS is zeroed.
//! 5. A minimal GDT is loaded.
//! 6. No IDT is loaded.
//!
//! The rest of the machine is in its reset state.
//!
//! In particular, we know very little about the virtual memory
//! mapping that we entered Rust with.  For example, we do not
//! presume that the page tables we are using are themselves
//! even in this mapping, writeable, etc.
//!
//! This code is responsible for:
//!
//! 1. Remapping the address space to properly place the loader
//!    and MMIO space with minimized virtual address mappings.
//! 2. Initializing the UART so that we can log errors.
//! 3. Setting up the IDT.
//! 4. Initializing a static data structure that describes the
//!    machine environment and returning it to the caller.  In
//!    particular, the bounds of the loader and MMIO regions are
//!    discovered here so that subsequent mappings do not
//!    overwrite the loader itself, its page tables, stack, or
//!    the MMIO regions.

extern crate alloc;

use crate::idt;
use crate::mem;
use crate::mmu;
use crate::uart::{self, Uart};
use alloc::boxed::Box;
use core::fmt;
use core::ops::Range;
use core::sync::atomic::{AtomicBool, Ordering};

#[cfg(not(test))]
core::arch::global_asm!(include_str!("start.S"), options(att_syntax));

/// The loader configuration, consumed by the rest of PHBL.
pub(crate) struct Config {
    pub(crate) cons: Uart,
    pub(crate) spchan: Uart,
    pub(crate) loader_region: Range<mem::V4KA>,
    pub(crate) mmio_region: Range<mem::V4KA>,
    pub(crate) page_table: mmu::LoaderPageTable,
}

impl fmt::Debug for Config {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        writeln!(f, "Config {{")?;
        writeln!(f, "    cons:   Uart({:x}),", self.cons.addr())?;
        writeln!(f, "    spchan: Uart({:x}),", self.spchan.addr())?;
        let vstart = self.loader_region.start.addr();
        let vend = self.loader_region.end.addr();
        writeln!(f, "    loader: {:#x?}", vstart..vend)?;
        let vstart = self.mmio_region.start.addr();
        let vend = self.mmio_region.end.addr();
        writeln!(f, "    mmio: {:#x?}", vstart..vend)?;
        writeln!(f, "    pageroot: P4KA({:#x}),", self.page_table.phys_addr())?;
        write!(f, "}}")
    }
}

/// Initializes the loader environment, creating the system
/// Config.  This remaps the kernel and MMIO region, respecting
/// segment permissions, etc.  Initializes the UART, and returns
/// a LoaderPageTable that we can use to create new mappings for
/// e.g. read and loading the host kernel.  This is called
/// directly from assembler code.
#[no_mangle]
pub(crate) unsafe extern "C" fn init(bist: u32) -> &'static mut Config {
    static INITED: AtomicBool = AtomicBool::new(false);
    if INITED.swap(true, Ordering::AcqRel) {
        panic!("Init already called");
    }
    uart::init();
    idt::init();
    if bist != 0 {
        panic!("bist failed: {:#x}", bist);
    }
    let page_table = remap();
    let loader_region = text_addr()..eaddr();
    let mmio_region = mmio_addr()..mmio_end();
    let reserved_regions = [loader_region.clone(), mmio_region.clone()];
    let config = Box::new(Config {
        cons: Uart::uart0(),
        spchan: Uart::uart1(),
        loader_region,
        mmio_region,
        page_table: mmu::LoaderPageTable::new(page_table, &reserved_regions),
    });
    Box::leak(config)
}

// Stubs for linker-provided symbols.
extern "C" {
    static __sloader: [u8; 0];
    static etext: [u8; 0];
    static erodata: [u8; 0];
    static edata: [u8; 0];
    static end: [u8; 0];
    static __eloader: [u8; 0];
    static bootblock: [u8; 0];

    pub fn dnr() -> !;
}

/// Returns the address of the start of the loader text segment.
fn text_addr() -> mem::V4KA {
    mem::V4KA::new(unsafe { __sloader.as_ptr() as usize })
}

/// Returns the address of the start of the loader read-only
/// data segment.
fn rodata_addr() -> mem::V4KA {
    mem::V4KA::new(unsafe { etext.as_ptr() as usize })
}

/// Returns the address of the start of the loader read/write
/// data segment.
fn data_addr() -> mem::V4KA {
    mem::V4KA::new(unsafe { erodata.as_ptr() as usize })
}

/// Returns the address of the start of the loader BSS segment.
fn bss_addr() -> mem::V4KA {
    mem::V4KA::new(unsafe { edata.as_ptr() as usize })
}

/// Returns the address of the end of the loader BSS.
fn end_addr() -> mem::V4KA {
    mem::V4KA::new(unsafe { end.as_ptr() as usize })
}

/// Returns the address of end of the loader memory image,
/// including the boot block and reset vector.
fn eaddr() -> mem::V4KA {
    mem::V4KA::new(unsafe { __eloader.as_ptr() as usize })
}

/// Returns the address of the boot block.
fn bootblock_addr() -> mem::V4KA {
    mem::V4KA::new(unsafe { bootblock.as_ptr() as usize })
}

/// Returns the address of the start of the MMIO region.
fn mmio_addr() -> mem::V4KA {
    mem::V4KA::new(0x8000_0000)
}

/// Returns the address of the end of the MMIO region.
fn mmio_end() -> mem::V4KA {
    mem::V4KA::new(0x1_0000_0000)
}

/// When the loader enters Rust code, we know that we have a
/// minimal virtual memory environment where the loader itself
/// is mapped rwx, and the UART registers region is mapped
/// rw- and uncached.  This remaps the loader and MMIO space
/// properly, enforcing appropriate protections for sections
/// and so on.
fn remap() -> &'static mut mmu::PageTable {
    let text = text_addr()..rodata_addr();
    let rodata = rodata_addr()..data_addr();
    let data = data_addr()..bss_addr();
    let bss = bss_addr()..end_addr();
    let boot = bootblock_addr()..eaddr();

    let mmio = mmio_addr()..mmio_end();

    let regions = &[
        mem::Region::new(text, mem::Attrs::new_text()),
        mem::Region::new(rodata, mem::Attrs::new_rodata()),
        mem::Region::new(data, mem::Attrs::new_data()),
        mem::Region::new(bss, mem::Attrs::new_bss()),
        mem::Region::new(boot, mem::Attrs::new_rodata()),
        mem::Region::new(mmio, mem::Attrs::new_mmio()),
    ];
    let page_table = mmu::PageTable::new();
    unsafe {
        page_table.identity_map(regions);
        page_table.activate()
    }
}

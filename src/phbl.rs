// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

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
#[cfg(not(any(test, clippy)))]
use alloc::boxed::Box;
use core::fmt;
use core::ops::Range;
use core::sync::atomic::{AtomicBool, Ordering};

#[cfg(not(test))]
core::arch::global_asm!(include_str!("start.S"), options(att_syntax));

/// The loader configuration, consumed by the rest of PHBL.
pub(crate) struct Config {
    pub(crate) cons: Uart,
    pub(crate) loader_region: Range<mem::V4KA>,
    pub(crate) page_table: mmu::LoaderPageTable,
}

impl fmt::Debug for Config {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        writeln!(f, "Config {{")?;
        writeln!(f, "    cons:   Uart({:x}),", self.cons.addr())?;
        let vstart = self.loader_region.start.addr();
        let vend = self.loader_region.end.addr();
        writeln!(f, "    loader: {:#x?}", vstart..vend)?;
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
        panic!("bist failed: {bist:#x}");
    }
    let cons = Uart::uart0();
    let page_table = remap(mem::V4KA::new(cons.addr()));
    let cpio_region = cpio_addr()..saddr();
    let loader_region = saddr()..eaddr();
    let mmio_region = mmio_addr()..mmio_end();
    let reserved_regions = [loader_region.clone(), cpio_region, mmio_region];
    let config = Box::new(Config {
        cons,
        loader_region,
        page_table: mmu::LoaderPageTable::new(page_table, &reserved_regions),
    });
    Box::leak(config)
}

// Stubs for linker-provided symbols.
extern "C" {
    static sbss: [u8; 0];
    static ebss: [u8; 0];
    static __sloader: [u8; 0];
    static etext: [u8; 0];
    static erodata: [u8; 0];
    static edata: [u8; 0];
    static __eloader: [u8; 0];
    static bootblock: [u8; 0];

    pub fn dnr() -> !;
}

/// Returns the address of the start of the cpio archive region.
fn cpio_addr() -> mem::V4KA {
    const CPIO_LEN: usize = 128 * mem::MIB;
    mem::V4KA::new(saddr().addr() - CPIO_LEN)
}

/// Returns the address of the start of the loader text segment.
fn text_addr() -> mem::V4KA {
    mem::V4KA::new(unsafe { __sloader.as_ptr().addr() })
}

/// Returns the address of the start of the loader read-only
/// data segment.
fn rodata_addr() -> mem::V4KA {
    mem::V4KA::new(unsafe { etext.as_ptr().addr() })
}

/// Returns the address of the start of the loader read/write
/// data segment.
fn data_addr() -> mem::V4KA {
    mem::V4KA::new(unsafe { erodata.as_ptr().addr() })
}

/// Returns the address of the end of the loader read/write
/// data segment.
fn edata_addr() -> mem::V4KA {
    mem::V4KA::new(unsafe { edata.as_ptr().addr() })
}

/// Returns the address of the start of the loader BSS segment.
fn bss_addr() -> mem::V4KA {
    mem::V4KA::new(unsafe { sbss.as_ptr().addr() })
}

/// Returns the address of the end of the loader BSS.
fn ebss_addr() -> mem::V4KA {
    mem::V4KA::new(unsafe { ebss.as_ptr().addr() })
}

/// Returns the start of the loader, including all segments.
fn saddr() -> mem::V4KA {
    bss_addr()
}

/// Returns the address of end of the loader memory image,
/// including the boot block and reset vector.
fn eaddr() -> mem::V4KA {
    mem::V4KA::new(unsafe { __eloader.as_ptr().addr() })
}

/// Returns the address of the boot block.
fn bootblock_addr() -> mem::V4KA {
    mem::V4KA::new(unsafe { bootblock.as_ptr().addr() })
}

/// Returns the address of the start of the MMIO region
/// containing the UART.
fn mmio_addr() -> mem::V4KA {
    mem::V4KA::new(0x8000_0000)
}

/// Returns the address of the end of the MMIO region containing
/// the UART.
fn mmio_end() -> mem::V4KA {
    mem::V4KA::new(0x1_0000_0000)
}

/// Returns a mutable slice over the cpio archive region.
/// Clears the region.
pub(crate) fn ramdisk_region_init_mut() -> &'static mut [u8] {
    let cpio = cpio_addr().addr();
    let ecpio = saddr().addr();
    const PHBL_MIN: usize = 2 * mem::GIB - 256 * mem::MIB;
    assert!(PHBL_MIN < cpio && cpio < ecpio);
    const PHBL_BASE: *mut u8 = core::ptr::without_provenance_mut(PHBL_MIN);
    let len = ecpio - cpio;
    let cpio = PHBL_BASE.with_addr(cpio);
    unsafe {
        core::ptr::write_bytes(cpio, 0, len);
        core::slice::from_raw_parts_mut(cpio, len)
    }
}

/// When the loader enters Rust code, we know that we have a
/// minimal virtual memory environment where the loader itself
/// is mapped rwx, and the UART registers region is mapped
/// rw- and uncached.  This remaps the loader and MMIO space
/// properly, enforcing appropriate protections for sections
/// and so on.
fn remap(cons_addr: mem::V4KA) -> &'static mut mmu::PageTable {
    let cpio = cpio_addr()..saddr();
    let text = text_addr()..rodata_addr();
    let rodata = rodata_addr()..data_addr();
    let data = data_addr()..edata_addr();
    let bss = bss_addr()..ebss_addr();
    let boot = bootblock_addr()..eaddr();

    let cons_end = mem::V4KA::new(cons_addr.addr() + mem::V4KA::SIZE);
    let cons = cons_addr..cons_end;

    let regions = &[
        mem::Region::new(cpio, mem::Attrs::new_data()),
        mem::Region::new(text, mem::Attrs::new_text()),
        mem::Region::new(rodata, mem::Attrs::new_rodata()),
        mem::Region::new(data, mem::Attrs::new_data()),
        mem::Region::new(bss, mem::Attrs::new_bss()),
        mem::Region::new(boot, mem::Attrs::new_rodata()),
        mem::Region::new(cons, mem::Attrs::new_mmio()),
    ];
    let page_table = mmu::PageTable::new();
    unsafe {
        page_table.identity_map(regions);
        page_table.activate()
    }
}

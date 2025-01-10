// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! Stub out things that are not ordinarily available in tests.
//! For instance, linker-provided symbols.

/// Linker symbol.
#[unsafe(no_mangle)]
static __sloader: usize = 4096;
/// Linker symbol.
#[unsafe(no_mangle)]
static etext: usize = 8192;
/// Linker symbol.
#[unsafe(no_mangle)]
static erodata: usize = 16384;
/// Linker symbol.
#[unsafe(no_mangle)]
static edata: usize = 32768;
/// Linker symbol.
#[unsafe(no_mangle)]
static sbss: usize = 32768 + 16384;
/// Linker symbol.
#[unsafe(no_mangle)]
static ebss: usize = 65536;
/// Defined in assembly.
#[unsafe(no_mangle)]
static stack: usize = 65536;
/// Defined in the loader.
#[unsafe(no_mangle)]
static bootblock: usize = 65536 + 4096;
/// Defined in the loader.
#[unsafe(no_mangle)]
static __eloader: usize = 65536 + 8192;
/// Defined in assembly.
#[unsafe(no_mangle)]
static MMIO_BASE: usize = 65536 + 16384;
/// Defined in assembly.
#[unsafe(no_mangle)]
static STACK_SIZE: u64 = 8 * 4096;
/// Defined in assembly.
#[unsafe(no_mangle)]
static GDT_CODE64: usize = 0x28;
/// Defined in assembly.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn dnr() {
    loop {}
}

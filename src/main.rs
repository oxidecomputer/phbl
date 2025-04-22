// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

#![feature(allocator_api)]
#![feature(naked_functions)]
#![feature(sync_unsafe_cell)]
#![cfg_attr(not(any(test, clippy)), no_std)]
#![cfg_attr(not(test), no_main)]
#![forbid(unsafe_op_in_unsafe_fn)]

mod allocator;
mod idt;
mod iomux;
mod loader;
mod mem;
mod mmu;
mod phbl;
mod uart;

type Result<T> = core::result::Result<T, &'static str>;

/// The main entry point, called from assembler.
#[unsafe(no_mangle)]
pub(crate) extern "C" fn entry(config: &mut phbl::Config) {
    println!();
    println!("Oxide Pico Host Boot Loader");
    println!("{config:#x?}");
    let ramdisk = expand_ramdisk();
    let kernel = find_kernel(ramdisk);
    let entry =
        loader::load(&mut config.page_table, kernel).expect("loaded kernel");
    println!("jumping into kernel...");
    entry(ramdisk.as_ptr().addr() as u64, ramdisk.len());
    panic!("main returning");
}

/// Expands the compressed cpio archive image (basically a
/// ramdisk) into a dedicated RAM region.  Note that the ramdisk
/// is compiled into the loader image.  Returns a slice around
/// the ramdisk contents.
fn expand_ramdisk() -> &'static [u8] {
    use miniz_oxide::inflate::TINFLStatus;
    use miniz_oxide::inflate::core::DecompressorOxide;
    use miniz_oxide::inflate::core::decompress;
    use miniz_oxide::inflate::core::inflate_flags::TINFL_FLAG_PARSE_ZLIB_HEADER;

    #[cfg(target_os = "none")]
    let cpio = include_bytes!(env!("PHBL_PHASE1_COMPRESSED_CPIO_ARCHIVE_PATH"));
    #[cfg(not(target_os = "none"))]
    let cpio = [0u8; 1];

    let dst = phbl::ramdisk_region_init_mut();
    let mut r = DecompressorOxide::new();
    let flags = TINFL_FLAG_PARSE_ZLIB_HEADER;
    print!(
        "Decompressing cpio archive to {:#x}..{:#x}...",
        dst.as_ptr().addr(),
        dst.len() + dst.as_ptr().addr(),
    );
    let (s, _, o) = decompress(&mut r, &cpio[..], dst, 0, flags);
    assert!(s == TINFLStatus::Done);
    println!("Done.");
    &dst[..o]
}

fn find_kernel(cpio: &[u8]) -> &[u8] {
    for entry in cpio_reader::iter_files(cpio) {
        if entry.name() == "platform/oxide/kernel/amd64/unix" {
            return entry.file();
        }
    }
    panic!("could not locate unix in cpio archive");
}

#[cfg(not(any(test, clippy)))]
mod no_std {
    #[panic_handler]
    pub fn panic(info: &core::panic::PanicInfo) -> ! {
        crate::println!("Panic: {:#?}", info);
        unsafe {
            crate::phbl::dnr();
        }
    }
}
#[cfg(test)]
mod fakes;

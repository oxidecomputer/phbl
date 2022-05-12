#![feature(allocator_api, alloc_error_handler, new_uninit)]
#![feature(asm_const, asm_sym)]
#![feature(naked_functions)]
#![cfg_attr(not(any(test, feature = "cargo-clippy")), no_std)]
#![cfg_attr(not(test), no_main)]
#![forbid(unsafe_op_in_unsafe_fn)]

mod allocator;
mod idt;
mod loader;
mod mem;
mod mmu;
mod phbl;
mod uart;

type Result<T> = core::result::Result<T, &'static str>;

/// The main entry point, called from assembler.
#[no_mangle]
pub(crate) extern "C" fn entry(config: &mut phbl::Config) {
    println!("Oxide Pico Host Boot Loader");
    println!("{config:#x?}");
    let bytes = read_kernel(&mut config.page_table);
    let entry =
        loader::load(&mut config.page_table, bytes).expect("loaded kernel");
    println!("jumping to kernel entry at {:#x?}", entry as *const fn());
    unsafe {
        entry();
    }
    panic!("main returning");
}

/// Reads the kernel image and ramdisk into RAM, decompressing
/// them on the way.
/// XXX(cross): These need to come from flash, not be compiled in.
fn read_kernel(page_table: &mut mmu::LoaderPageTable) -> &'static [u8] {
    use miniz_oxide::inflate::core::decompress;
    use miniz_oxide::inflate::core::inflate_flags::TINFL_FLAG_PARSE_ZLIB_HEADER;
    use miniz_oxide::inflate::core::DecompressorOxide;
    use miniz_oxide::inflate::TINFLStatus;

    let k = include_bytes!("/home/cross/k.z");
    let rd = include_bytes!("/home/cross/rd.z");

    const RAMDISK_OFFSET: usize = 16 * 1024 * 1024;
    const LEN: usize = RAMDISK_OFFSET + 64 * 1024 * 1024;
    const DST_ADDR: mem::V4KA = mem::V4KA::new(0x1_0000_0000);
    const DST_END: mem::V4KA = mem::V4KA::new(DST_ADDR.addr() + LEN);
    let dst = unsafe {
        page_table
            .map_region(
                DST_ADDR..DST_END,
                mem::Attrs::new_data(),
                mem::P4KA::new(DST_ADDR.addr() as u64),
            )
            .expect("cannot map ramdisk save region");
        let p = DST_ADDR.addr() as *mut u8;
        core::ptr::write_bytes(p, 0, LEN);
        core::slice::from_raw_parts_mut(p, LEN)
    };
    let (kdst, rddst) = dst.split_at_mut(RAMDISK_OFFSET);
    let mut r = DecompressorOxide::new();
    let flags = TINFL_FLAG_PARSE_ZLIB_HEADER;
    print!(
        "Decompressing kernel image to {:#x?}..{:#x?}...",
        kdst.as_ptr(),
        kdst.len() + kdst.as_ptr() as usize,
    );
    let (s, _, o) = decompress(&mut r, &k[..], kdst, 0, flags);
    assert!(s == TINFLStatus::Done);
    println!("Done.");
    let kernel = &kdst[..o];
    let mut r = DecompressorOxide::new();
    print!(
        "Decompressing RAMDISK image to {:#x?}..{:#x}...",
        rddst.as_ptr(),
        rddst.len() + rddst.as_ptr() as usize,
    );
    let (s, _, _) = decompress(&mut r, &rd[..], rddst, 0, flags);
    assert!(s == TINFLStatus::Done);
    println!("Done.");
    kernel
}

#[cfg(not(any(test, feature = "cargo-clippy")))]
mod no_std {
    #[panic_handler]
    pub extern "C" fn panic(info: &core::panic::PanicInfo) -> ! {
        crate::println!("Panic: {:#?}", info);
        unsafe {
            crate::phbl::dnr();
        }
    }
}
#[cfg(test)]
mod fakes;

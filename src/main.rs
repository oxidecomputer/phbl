#![feature(lang_items)]
#![cfg_attr(not(test), no_std)]
#![cfg_attr(not(test), no_main)]

#[no_mangle]
pub extern "C" fn entry() {}

#[cfg(not(test))]
mod no_std {
    #[panic_handler]
    pub extern "C" fn panic(_info: &core::panic::PanicInfo) -> ! {
        #[allow(clippy::empty_loop)]
        loop {}
    }

    #[lang = "eh_personality"]
    extern "C" fn eh_personality() {}
}

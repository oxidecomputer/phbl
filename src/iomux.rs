// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

/// Supported pin functions.
#[derive(Clone, Copy)]
#[repr(u8)]
enum GpioX {
    F0 = 0b00,
    _F1 = 0b01,
    _F2 = 0b10,
    _F3 = 0b11,
}

/// Initializes the IO mux so that pins for UART 0 are mapped to
/// UART functions.  In some cases, the values observed are
/// different from the documented reset values, so we force them
/// to our desired settings.
///
/// # Safety
/// The caller must ensure that the IO mux MMIO region is in the
/// current address space.
pub unsafe fn init() {
    if let Some(settings) = mux_settings() {
        use core::ptr;
        const GPIO_BASE_ADDR: usize = 0xFED8_0000;
        const IOMUX_BASE_ADDR: usize = GPIO_BASE_ADDR + 0x0D00;
        let iomux = ptr::with_exposed_provenance_mut::<u8>(IOMUX_BASE_ADDR);
        for (pin, function) in settings.iter() {
            unsafe {
                ptr::write_volatile(iomux.offset(*pin), *function as u8);
            }
        }
    }
}

/// Returns the correct IO mux settings for the current system,
/// if any.
fn mux_settings() -> Option<&'static [(isize, GpioX)]> {
    const SP5: u32 = 4;

    match cpuinfo()? {
        // We really ought to explicitly check socket type
        // for the earlier processor models here.
        (0x17, 0x00..=0x0f, 0x0..=0xf, _) | // Naples
        (0x17, 0x30..=0x3f, 0x0..=0xf, _) | // Rome
        (0x19, 0x00..=0x0f, 0x0..=0xf, _) | // Milan
        (0x19, 0x10..=0x1f, 0x0..=0xf, Some(SP5)) | // Genoa
        (0x19, 0xa0..=0xaf, 0x0..=0xf, Some(SP5)) | // Bergamo and Sienna
        (0x1a, 0x00..=0x1f, 0x0..=0xf, Some(SP5)) => { // Turin
            Some(&[
                (135, GpioX::F0),
                (136, GpioX::F0),
                (137, GpioX::F0),
                (138, GpioX::F0),
            ])
        },
        _ => None,
    }
}

/// Returns information about the current processor and its
/// package.
fn cpuinfo() -> Option<(u8, u8, u8, Option<u32>)> {
    let cpuid = x86::cpuid::CpuId::new();
    let features = cpuid.get_feature_info()?;
    let family = features.family_id();
    let ext = cpuid.get_extended_processor_and_feature_identifiers()?;
    let pkg_type = (family > 0x10).then_some(ext.pkg_type());
    Some((family, features.model_id(), features.stepping_id(), pkg_type))
}

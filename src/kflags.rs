// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use bitstruct::bitstruct;

/// The type of system we are booting.
/// Options include:
/// * Oxide: A Gimlet or Cosmo; startup information taken from IPCC
/// * Ethanol-X: An AMD Ethanol-X reference design machine
/// * Ruby: An AMD Ruby reference design machine
/// * RubyRed: An AMD Ruby machine with Grapefruit board
#[derive(Clone, Copy, Debug)]
#[repr(u8)]
pub enum Machine {
    Oxide = 0,
    EthanolX = 1,
    Ruby = 2,
    RubyRed = 3,
}

bitstruct! {
    pub struct KFlags(u64) {
        pub recovery: bool = 0;
        pub kbm: bool = 1;
        pub bootrd: bool = 2;
        pub prom: bool = 3;
        pub kmdb: bool = 4;
        pub kmdb_boot: bool = 5;
        pub ramdisk: bool = 6;
        pub netboot: bool = 7;
        pub verbose: bool = 8;
        // The raw machine type is the bit representation
        // of a value from the Machine enumeration.
        pub machine: Machine = 56..64;
    }
}

impl KFlags {
    pub fn new() -> KFlags {
        KFlags(0)
    }

    pub fn bits(self) -> u64 {
        self.0
    }
}

impl bitstruct::IntoRaw<u8, Machine> for KFlags {
    fn into_raw(machine: Machine) -> u8 {
        machine as u8
    }
}

impl bitstruct::FromRaw<u8, Machine> for KFlags {
    fn from_raw(raw: u8) -> Machine {
        match raw {
            0 => Machine::Oxide,
            1 => Machine::EthanolX,
            2 => Machine::Ruby,
            3 => Machine::RubyRed,
            _ => panic!("bad machine type: {raw}"),
        }
    }
}

pub(crate) fn flags() -> KFlags {
    let flags = KFlags::new();
    #[cfg(any(feature = "ethx", feature = "ruby", feature = "ruby_red"))]
    let flags = flags.with_machine(
        #[cfg(feature = "ethx")]
        Machine::EthanolX,
        #[cfg(feature = "ruby")]
        Machine::Ruby,
        #[cfg(feature = "ruby_red")]
        Machine::RubyRed,
    );
    #[cfg(feature = "recovery")]
    let flags = flags.with_recovery(true);
    #[cfg(feature = "kbm_debug")]
    let flags = flags.with_kbm(true);
    #[cfg(feature = "bootrd_debug")]
    let flags = flags.with_bootrd(true);
    #[cfg(feature = "prom_debug")]
    let flags = flags.with_prom(true);
    #[cfg(feature = "kmdb")]
    let flags = flags.with_kmdb(true);
    #[cfg(feature = "kmdb_boot")]
    let flags = flags.with_kmdb_boot(true);
    #[cfg(feature = "boot_ramdisk")]
    let flags = flags.with_ramdisk(true);
    #[cfg(feature = "boot_net")]
    let flags = flags.with_netboot(true);
    #[cfg(feature = "boot_verbose")]
    let flags = flags.with_verbose(true);

    #[allow(clippy::let_and_return)]
    flags
}

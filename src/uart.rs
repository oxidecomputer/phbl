// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! Synopsis DesignWare Advanced Peripheral Bus UART driver
//!
//! AMD EPYC processors have an AMBA UART built around the
//! Synopsis DesignWare APB UART part.  This is largely NS16550
//! compatible, but accessed via MMIO; registers are aligned on
//! 32-bit boundaries.
//!
//! Milan supports 4 UARTs; the first two support flow control
//! if the last two are disabled.

use bitstruct::bitstruct;
use core::fmt;
use core::ptr;
use core::sync::atomic::{AtomicBool, Ordering};
use static_assertions::const_assert_eq;

bitstruct! {
    /// Receive buffer register
    #[derive(Clone, Copy)]
    pub struct Rbr(u32) {
        data: u8 = 0..8;
    }
}

bitstruct! {
    /// Transmit hold register.
    pub struct Thr(u32) {
        data: u8 = 0..8;
    }
}

bitstruct! {
    /// Divisor latch low
    pub struct Dll(u32) {
        lo: u8 = 0..8;
    }
}

bitstruct! {
    /// Divisor latch hi
    pub struct Dlh(u32) {
        hi: u8 = 0..8;
    }
}

enum RcvrTrigger {
    One = 0b00,
    Quarter = 0b01,
    Half = 0b10,
    Less2 = 0b11,
}

enum TxEmptyTrigger {
    Empty = 0b00,
    Two = 0b01,
    Quarter = 0b10,
    Half = 0b11,
}

bitstruct! {
    /// FIFO control register
    pub struct Fcr(u32) {
        enable: bool = 0;
        rcvr_fifo_reset: bool = 1;
        xmtr_fifo_reset: bool = 2;
        tx_empty_trigger: TxEmptyTrigger = 4..=5;
        rcvr_trigger: RcvrTrigger = 6..=7;
    }
}

impl bitstruct::FromRaw<u8, TxEmptyTrigger> for Fcr {
    fn from_raw(raw: u8) -> TxEmptyTrigger {
        match raw {
            0b00 => TxEmptyTrigger::Empty,
            0b01 => TxEmptyTrigger::Two,
            0b10 => TxEmptyTrigger::Quarter,
            0b11 => TxEmptyTrigger::Half,
            _ => panic!("impossible data bits value"),
        }
    }
}

impl bitstruct::IntoRaw<u8, TxEmptyTrigger> for Fcr {
    fn into_raw(bits: TxEmptyTrigger) -> u8 {
        bits as u8
    }
}

impl bitstruct::FromRaw<u8, RcvrTrigger> for Fcr {
    fn from_raw(raw: u8) -> RcvrTrigger {
        match raw {
            0b00 => RcvrTrigger::One,
            0b01 => RcvrTrigger::Quarter,
            0b10 => RcvrTrigger::Half,
            0b11 => RcvrTrigger::Less2,
            _ => panic!("impossible data bits value"),
        }
    }
}

impl bitstruct::IntoRaw<u8, RcvrTrigger> for Fcr {
    fn into_raw(bits: RcvrTrigger) -> u8 {
        bits as u8
    }
}

enum Datas {
    Bits5 = 0b00,
    Bits6 = 0b01,
    Bits7 = 0b10,
    Bits8 = 0b11,
}

enum Parity {
    No,
    DisabledEven,
    Odd,
    Even,
}

enum Stops {
    Stop1,
    Stop2,
}

#[repr(u32)]
enum Rate {
    B3M = 3_000_000u32,
}

bitstruct! {
    /// Line control register.
    #[derive(Clone, Copy)]
    pub struct Lcr(u32) {
        data_bits: Datas = 0..=1;
        stop: Stops = 2;
        parity: Parity = 3..=4;
        dlab: bool = 7;
    }
}

impl bitstruct::FromRaw<u8, Datas> for Lcr {
    fn from_raw(raw: u8) -> Datas {
        match raw {
            0b00 => Datas::Bits5,
            0b01 => Datas::Bits6,
            0b10 => Datas::Bits7,
            0b11 => Datas::Bits8,
            _ => panic!("impossible data bits value"),
        }
    }
}

impl bitstruct::IntoRaw<u8, Datas> for Lcr {
    fn into_raw(bits: Datas) -> u8 {
        bits as u8
    }
}

impl bitstruct::FromRaw<bool, Stops> for Lcr {
    fn from_raw(raw: bool) -> Stops {
        match raw {
            false => Stops::Stop1,
            true => Stops::Stop2,
        }
    }
}

impl bitstruct::IntoRaw<bool, Stops> for Lcr {
    fn into_raw(bits: Stops) -> bool {
        match bits {
            Stops::Stop1 => false,
            Stops::Stop2 => true,
        }
    }
}

impl bitstruct::FromRaw<u8, Parity> for Lcr {
    fn from_raw(raw: u8) -> Parity {
        match raw {
            0b00 => Parity::No,
            0b01 => Parity::DisabledEven,
            0b10 => Parity::Odd,
            0b11 => Parity::Even,
            _ => panic!("impossible data bits value"),
        }
    }
}

impl bitstruct::IntoRaw<u8, Parity> for Lcr {
    fn into_raw(parity: Parity) -> u8 {
        match parity {
            Parity::No => 0b00,
            Parity::DisabledEven => 0b01,
            Parity::Odd => 0b10,
            Parity::Even => 0b11,
        }
    }
}

bitstruct! {
    /// Ill-named Modem Control Register
    struct Mcr(u32) {
        dtr: bool = 0;
        rts: bool = 1;
        // out1: bool = 2;
        out2: bool = 3;
        // loopback: bool = 4;
        auto_flow: bool = 5;
    }
}

bitstruct! {
    /// Line Status Register
    struct Lsr(u32) {
        data_ready: bool = 0;
        overrun_err: bool = 1;
        parity_err: bool = 2;
        framing_err: bool = 3;
        break_intr: bool = 4;
        thr_empty: bool = 5;
        xmit_empty: bool = 6;
        rcv_fifo_err: bool = 7;
    }
}

bitstruct! {
    /// Software Reset Register
    struct Srr(u32) {
        uart_reset: bool = 0;
        rcvr_fifo_reset: bool = 1;
        xmtr_fifo_reset: bool = 2;
    }
}

bitstruct! {
    /// UART Status Register
    struct Usr(u32) {
        busy: bool = 0;
        tx_fifo_not_full: bool = 1;
        tx_fifo_empty: bool = 2;
        rx_fifo_not_empty: bool = 3;
        rx_fifo_full: bool = 4;
    }
}

/// The base virtual address of all UARTs.
const UART_MMIO_BASE_ADDR: usize = 0xFEDC_9000;

/// Describes the UART registers when the divisor latch is set
/// in the line control register.  This is the state in that
/// that the UART is in after calling Device::reset.
#[repr(C)]
struct ConfigMmio {
    dll: Dll,
    dlh: Dlh,
    fcr: Fcr,
    lcr: Lcr,
    mcr: Mcr,
    _res: [u32; 29],
    srr: Srr,
    _rest: [u32; 29],
}
const_assert_eq!(core::mem::size_of::<ConfigMmio>(), 256);

impl ConfigMmio {
    fn lcr(&self) -> Lcr {
        unsafe { ptr::read_volatile(&self.lcr) }
    }

    /// Sets the line rate on the device.
    fn set_rate(&mut self, rate: Rate) {
        const SCLK: u32 = 48_000_000;
        let divisor = SCLK / (16 * rate as u32);
        let dll = Dll(divisor & 0xFF);
        let dlh = Dlh(divisor >> 8);
        unsafe {
            let lcr = self.lcr().with_dlab(true);
            ptr::write_volatile(&mut self.lcr, lcr);
            ptr::write_volatile(&mut self.dll, dll);
            ptr::write_volatile(&mut self.dlh, dlh);
            let lcr = self.lcr().with_dlab(false);
            ptr::write_volatile(&mut self.lcr, lcr);
        }
    }

    fn set_data_bits(&mut self, data: Datas) {
        unsafe {
            let lcr = self.lcr().with_data_bits(data);
            ptr::write_volatile(&mut self.lcr, lcr);
        }
    }

    fn set_stop_bits(&mut self, stop: Stops) {
        unsafe {
            let lcr = self.lcr().with_stop(stop);
            ptr::write_volatile(&mut self.lcr, lcr);
        }
    }

    fn set_parity(&mut self, parity: Parity) {
        unsafe {
            let lcr = self.lcr().with_parity(parity);
            ptr::write_volatile(&mut self.lcr, lcr);
        }
    }

    fn config_flow_control(&mut self) {
        let mcr = Mcr(0)
            .with_auto_flow(true)
            .with_out2(true)
            .with_rts(true)
            .with_dtr(true);
        unsafe {
            ptr::write_volatile(&mut self.mcr, mcr);
        }
    }

    fn config_fifos(&mut self) {
        let fcr = Fcr(0)
            .with_enable(true)
            .with_rcvr_fifo_reset(true)
            .with_xmtr_fifo_reset(true)
            .with_tx_empty_trigger(TxEmptyTrigger::Quarter)
            .with_rcvr_trigger(RcvrTrigger::Half);
        unsafe {
            ptr::write_volatile(&mut self.fcr, fcr);
        }
    }
}

/// Describes the UART registers for a read
#[repr(C)]
struct MmioRead {
    rbr: Rbr,         // 0x00
    _ier: u32,        // 0x04
    _iir: u32,        // 0x08
    _lcr: u32,        // 0x0C
    _mcr: u32,        // 0x10
    _lsr: Lsr,        // 0x14
    _msr: u32,        // 0x18
    _scr: u32,        // 0x1C
    _lpdll: u32,      // 0x20
    _lpdlh: u32,      // 0x24
    _res0: u32,       // 0x28
    _res1: u32,       // 0x2C
    _sdat: [u32; 16], // 0x30 - 0x6C    // srbr and sthr
    _far: u32,        // 0x70
    _tfr: u32,        // 0x74
    _rfw: u32,        // 0x78
    usr: Usr,         // 0x7C
    _tfl: u32,        // 0x80
    _rfl: u32,        // 0x84
    _srr: u32,        // 0x88
    _srts: u32,       // 0x8C,
    _sbcr: u32,       // 0x90
    _sdmam: u32,      // 0x94
    _sfe: u32,        // 0x98
    _srt: u32,        // 0x9C
    _stet: u32,       // 0xA0
    _htx: u32,        // 0xA4
    _dmasa: u32,      // 0xA8
    _res2: [u32; 18], // 0xAC - 0xF0
    _cpr: u32,        // 0xF4
    _ucv: u32,        // 0xF8
    _ctr: u32,        // 0xFC
}
const_assert_eq!(core::mem::size_of::<MmioRead>(), 256);

/// Describes the UART registers for a write
#[repr(C)]
struct MmioWrite {
    thr: Thr,         // 0x00
    _ier: u32,        // 0x04
    _iir: u32,        // 0x08
    _lcr: u32,        // 0x0C
    _mcr: u32,        // 0x10
    _lsr: Lsr,        // 0x14
    _msr: u32,        // 0x18
    _scr: u32,        // 0x1C
    _lpdll: u32,      // 0x20
    _lpdlh: u32,      // 0x24
    _res0: u32,       // 0x28
    _res1: u32,       // 0x2C
    _sdat: [u32; 16], // 0x30 - 0x6C    // srbr and sthr
    _far: u32,        // 0x70
    _tfr: u32,        // 0x74
    _rfw: u32,        // 0x78
    usr: Usr,         // 0x7C
    _tfl: u32,        // 0x80
    _rfl: u32,        // 0x84
    _srr: u32,        // 0x88
    _srts: u32,       // 0x8C,
    _sbcr: u32,       // 0x90
    _sdmam: u32,      // 0x94
    _sfe: u32,        // 0x98
    _srt: u32,        // 0x9C
    _stet: u32,       // 0xA0
    _htx: u32,        // 0xA4
    _dmasa: u32,      // 0xA8
    _res2: [u32; 18], // 0xAC - 0xF0
    _cpr: u32,        // 0xF4
    _ucv: u32,        // 0xF8
    _ctr: u32,        // 0xFC
}
const_assert_eq!(core::mem::size_of::<MmioWrite>(), 256);

/// Represents a specific UART and its base MMIO address.
#[derive(Clone, Copy, Debug)]
#[repr(usize)]
pub enum Device {
    Uart0 = UART_MMIO_BASE_ADDR,
    _Uart1 = UART_MMIO_BASE_ADDR + 0x1000,
    _Uart2 = UART_MMIO_BASE_ADDR + 0x5000,
    _Uart3 = UART_MMIO_BASE_ADDR + 0x6000,
}

static UART0_INITED: AtomicBool = AtomicBool::new(false);
static UART1_INITED: AtomicBool = AtomicBool::new(false);
static UART2_INITED: AtomicBool = AtomicBool::new(false);
static UART3_INITED: AtomicBool = AtomicBool::new(false);

impl Device {
    /// Returns the base virtual address of the device's
    /// MMIO region.
    fn addr(self) -> usize {
        self as usize
    }

    fn init(self, rate: Rate, data: Datas, stop: Stops, par: Parity) -> bool {
        let uart = self.reset();
        uart.set_rate(rate);
        uart.set_data_bits(data);
        uart.set_stop_bits(stop);
        uart.set_parity(par);
        uart.config_flow_control();
        uart.config_fifos();
        true
    }

    fn reset<'a>(self) -> &'a mut ConfigMmio {
        let regs = core::ptr::from_exposed_addr_mut::<ConfigMmio>(self.addr());
        let uart = unsafe { &mut *regs };
        unsafe {
            ptr::write_volatile(
                &mut uart.srr,
                Srr(0)
                    .with_uart_reset(true)
                    .with_rcvr_fifo_reset(true)
                    .with_xmtr_fifo_reset(true),
            );
        }
        uart
    }
}

/// The UART itself.
pub struct Uart(Device);

impl Uart {
    pub fn uart0() -> Uart {
        assert!(UART0_INITED.load(Ordering::Acquire));
        Uart(Device::Uart0)
    }

    pub(crate) fn addr(&self) -> usize {
        self.0.addr()
    }

    fn write_mmio_mut(&mut self) -> &mut MmioWrite {
        let regs = core::ptr::from_exposed_addr_mut::<MmioWrite>(self.0.addr());
        unsafe { &mut *regs }
    }

    // Note that reading from the device alters its state.  We
    // model that by returning a mut ref.  This also means that
    // it is mutually exclusive with a write MMIO structure,
    // as the two share the same register space.
    fn read_mmio_mut(&mut self) -> &mut MmioRead {
        let regs = core::ptr::from_exposed_addr_mut::<MmioRead>(self.0.addr());
        unsafe { &mut *regs }
    }

    #[allow(dead_code)]
    fn getb(&mut self) -> u8 {
        while {
            let usr = unsafe { ptr::read_volatile(&self.read_mmio_mut().usr) };
            !usr.rx_fifo_not_empty()
        } {
            core::hint::spin_loop();
        }
        let data = unsafe { ptr::read_volatile(&self.read_mmio_mut().rbr) };
        data.data()
    }

    fn putb(&mut self, b: u8) {
        while {
            let usr = unsafe { ptr::read_volatile(&self.write_mmio_mut().usr) };
            !usr.tx_fifo_not_full()
        } {
            // We're not racing against anyone, but, this doesn't hurt.
            core::hint::spin_loop();
        }
        let data = Thr(0).with_data(b);
        unsafe {
            ptr::write_volatile(&mut self.write_mmio_mut().thr, data);
        }
    }
}

/// Returns the (initialized) UART device used for the logging
/// console.
pub fn cons() -> Uart {
    Uart::uart0()
}

/// Initializes the console UART.
///
/// # Safety
/// The caller must ensure that MMIO space for the UARTs is
/// properly mapped before calling this.
pub fn init() {
    if !UART0_INITED.swap(true, Ordering::AcqRel) {
        Device::Uart0.init(Rate::B3M, Datas::Bits8, Stops::Stop1, Parity::No);
    }
    UART1_INITED.store(false, Ordering::Release);
    UART2_INITED.store(false, Ordering::Release);
    UART3_INITED.store(false, Ordering::Release);
}

/// By implementing `Write` on the UART, we can implement the
/// formatted output functions.
impl fmt::Write for Uart {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        for b in s.bytes() {
            if b == b'\n' {
                self.putb(b'\r');
            }
            self.putb(b);
        }
        Ok(())
    }
}

/// A simple println!().
#[macro_export]
macro_rules! println {
    () => ($crate::print!("\n"));
    ($($arg:tt)*) => ($crate::print!("{}\n", format_args!($($arg)*)));
}

#[macro_export]
macro_rules! print {
    ($($args:tt)*) => ({
        use core::fmt::Write;
        let mut cons = $crate::uart::cons();
        cons.write_fmt(format_args!($($args)*)).unwrap();
    })
}

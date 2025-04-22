// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

// Derived from the rxv64 operating system.

use crate::println;
use bit_field::BitField;
use bitstruct::bitstruct;
use core::arch::{asm, naked_asm};
use core::ptr;
use seq_macro::seq;

/// Returns the selector for the 64-bit code segment in the GDT.
fn code64() -> u16 {
    unsafe extern "C" {
        static GDT_CODE64: [u8; 0]; // The selector as an absolute symbol.
    }
    unsafe { GDT_CODE64.as_ptr() as u16 }
}

bitstruct! {
    /// An interrupt gate descriptor.
    #[derive(Clone, Copy, Default)]
    pub struct GateDesc(u128) {
        pub offset0: u16 = 0..16;
        pub segment_selector: u16 = 16..32;
        pub stack_table_index: u8 = 32..35;
        mbz0: bool = 35;
        mbz1: bool = 36;
        mbz2: u8 = 37..40;
        pub fixed_type: u8 = 40..44;
        mbz3: bool = 44;
        pub privilege_level: u8 = 45..47;
        pub present: bool = 47;
        pub offset16: u16 = 48..64;
        pub offset32: u32 = 64..96;
        reserved: u32 = 96..128;
    }
}

impl GateDesc {
    /// Returns an empty interrupt gate descriptor.
    pub const fn empty() -> GateDesc {
        const TYPE_INTERRUPT_GATE: u8 = 0b1110;
        GateDesc(0).with_fixed_type(TYPE_INTERRUPT_GATE)
    }

    /// Returns a new interrupt gate descriptor that dispatches
    /// to the given thunk.
    pub fn new(thunk: unsafe extern "C" fn() -> !) -> GateDesc {
        const DPL_KERN: u8 = 0b00;
        const RSP0: u8 = 0;
        let va = thunk as usize;
        GateDesc::empty()
            .with_offset0(va.get_bits(0..16) as u16)
            .with_offset16(va.get_bits(16..32) as u16)
            .with_offset32(va.get_bits(32..64) as u32)
            .with_stack_table_index(RSP0)
            .with_segment_selector(code64())
            .with_present(true)
            .with_privilege_level(DPL_KERN)
    }
}

/// The trap frame captured by software on exceptions
#[derive(Copy, Clone, Debug)]
#[repr(C)]
struct TrapFrame {
    // Pushed by software.
    rax: u64,
    rbx: u64,
    rcx: u64,
    rdx: u64,
    rsi: u64,
    rdi: u64,
    rbp: u64,
    r8: u64,
    r9: u64,
    r10: u64,
    r11: u64,
    r12: u64,
    r13: u64,
    r14: u64,
    r15: u64,

    // %ds and %es are not used in 64-bit mode, but they exist,
    // so we save and restore them.
    ds: u64, // Really these are u16s, but
    es: u64, // we waste a few bytes to keep
    fs: u64, // the stack aligned.  Thank
    gs: u64, // you, x86 segmentation.

    vector: u64,

    // Sometimes pushed by hardware.
    error: u64,

    // Pushed by hardware.
    rip: u64,
    cs: u64,
    rflags: u64,
    rsp: u64,
    ss: u64,
}

macro_rules! gen_stub {
    ($name:ident, $vecnum:expr) => {
        #[unsafe(naked)]
        unsafe extern "C" fn $name() -> ! {
            naked_asm!("pushq $0; pushq ${}; jmp {}",
                const $vecnum, sym alltraps,
                options(att_syntax))
        }
    };
    ($name:ident, $vecnum:expr, err) => {
        #[unsafe(naked)]
        unsafe extern "C" fn $name() -> ! {
            naked_asm!("pushq ${}; jmp {}",
                const $vecnum, sym alltraps,
                options(att_syntax))
        }
    };
}

macro_rules! gen_vector_stub {
    // These cases include hardware-generated error words
    // on the trap frame.
    (vector8, 8) => {
        gen_stub!(vector8, 8, err);
    };
    (vector10, 10) => {
        gen_stub!(vector10, 10, err);
    };
    (vector11, 11) => {
        gen_stub!(vector11, 11, err);
    };
    (vector12, 12) => {
        gen_stub!(vector12, 12, err);
    };
    (vector13, 13) => {
        gen_stub!(vector13, 13, err);
    };
    (vector14, 14) => {
        gen_stub!(vector14, 14, err);
    };
    (vector17, 17) => {
        gen_stub!(vector17, 17, err);
    };
    // No hardware error.
    ($vector:ident, $num:expr) => {
        gen_stub!($vector, $num);
    };
}

seq!(N in 0..=255 {
    gen_vector_stub!(vector~N, N);
});

/// The common trap routine that all vectors dispatch to.  Saves
/// hardware state and invokes `trap`.  Note that the vector
/// number and a padding zero for exceptions that don't push a
/// hardware error are pushed by the trap stubs before jumping
/// here.
#[unsafe(naked)]
unsafe extern "C" fn alltraps() -> ! {
    naked_asm!(r#"
        // Save the x86 segmentation registers.
        subq $32, %rsp
        movq $0, 24(%rsp);
        movw %gs, 24(%rsp);
        movq $0, 16(%rsp);
        movw %fs, 16(%rsp);
        movq $0, 8(%rsp);
        movw %es, 8(%rsp);
        movq $0, (%rsp);
        movw %ds, (%rsp);
        pushq %r15;
        pushq %r14;
        pushq %r13;
        pushq %r12;
        pushq %r11;
        pushq %r10;
        pushq %r9;
        pushq %r8;
        pushq %rbp;
        pushq %rdi;
        pushq %rsi;
        pushq %rdx;
        pushq %rcx;
        pushq %rbx;
        pushq %rax;
        movq %rsp, %rdi;
        callq {trap};
        popq %rax;
        popq %rbx;
        popq %rcx;
        popq %rdx;
        popq %rsi;
        popq %rdi;
        popq %rbp;
        popq %r8;
        popq %r9;
        popq %r10;
        popq %r11;
        popq %r12;
        popq %r13;
        popq %r14;
        popq %r15;
        movw (%rsp), %ds;
        movw 8(%rsp), %es;
        movw 16(%rsp), %fs;
        movw 24(%rsp), %gs;
        addq $32, %rsp;
        // Pop vector and error word.
        addq $16, %rsp;
        iretq;
        "#,
        trap = sym trap,
        options(att_syntax))
}

/// The Interrupt Descriptor Table.
#[repr(C, align(4096))]
struct Idt {
    entries: [GateDesc; 256],
}

impl Idt {
    /// Returns an empty IDT for initializing the static global.
    const fn empty() -> Idt {
        Idt { entries: [GateDesc::empty(); 256] }
    }

    /// Initializes the IDT by writing the gates to refer to the
    /// vector stub routines.
    fn init(&mut self) {
        self.entries = seq!(N in 0..=255 {
            [#(
                GateDesc::new(vector~N),
            )*]
        });
    }

    /// Loads the IDT into the CPU.  Creates an IDT descriptor
    /// on the stack and invokes the `lidt` instruction.
    unsafe fn load(&'static mut self) {
        const LIMIT: u16 = core::mem::size_of::<Idt>() as u16 - 1;
        unsafe {
            asm!(r#"
                subq $16, %rsp;
                movq {}, 8(%rsp);
                movw ${}, 6(%rsp);
                lidt 6(%rsp);
                addq $16, %rsp;
                "#, in(reg) self, const LIMIT, options(att_syntax));
        }
    }
}

extern "C" fn trap(frame: &mut TrapFrame) {
    println!("Exception:");
    println!("{frame:#x?}");
    println!("cr0: {:#x}", unsafe { x86::controlregs::cr0() });
    println!("cr2: {:#x}", unsafe { x86::controlregs::cr2() });
    println!("cr3: {:#x}", unsafe { x86::controlregs::cr3() });
    println!("cr4: {:#x}", unsafe { x86::controlregs::cr4() });
    println!("efer: {:#x}", unsafe { x86::msr::rdmsr(x86::msr::IA32_EFER) });
    unsafe {
        backtrace(frame.rbp);
    }
    // Arrange for the exception return to land in a halt loop.
    // The seemingly superfluous cast to usize and then again to
    // u64 keeps clippy happy.
    frame.rip = crate::phbl::dnr as usize as u64;
}

/// Prints a call backtrace starting from the given frame
/// pointer.
///
/// # Safety
/// Be sure to call this with something you are fairly certain
/// is a valid stack frame that does not alias the current stack.
unsafe fn backtrace(mut rbp: u64) {
    unsafe extern "C" {
        static stack: [u8; 0];
        static STACK_SIZE: [u8; 0]; // Really the size, but an absolute symbol
    }
    let base = unsafe { stack.as_ptr() } as *const u64;
    let sstack = base.addr() as u64;
    let estack = sstack + unsafe { STACK_SIZE.as_ptr().addr() as u64 };
    println!("stack [{sstack:x}..{estack:x}) %rip trace:");
    while rbp != 0 {
        if rbp < sstack || estack < rbp + 16 || rbp & 0b1111 != 0 {
            println!(
                "bogus frame pointer {rbp:#x} (stack {sstack:#x}..{estack:#x})"
            );
            break;
        }
        let p = base.with_addr(rbp as usize);
        let next_rbp = unsafe { ptr::read(p) };
        if next_rbp != 0 && next_rbp < rbp + 16 {
            println!("stack is corrupt {next_rbp:#x} <= {rbp:#x}");
            break;
        }
        let rip = unsafe { ptr::read(p.add(1)) };
        rbp = next_rbp;
        println!("{rip:#x}");
    }
}

/// Initialize and load the IDT.
/// Should be called exactly once, early in boot.
pub(crate) fn init() {
    use core::cell::SyncUnsafeCell;
    use core::sync::atomic::{AtomicBool, Ordering};
    static INITED: AtomicBool = AtomicBool::new(false);
    if INITED.swap(true, Ordering::AcqRel) {
        panic!("IDT already initialized");
    }
    static IDT: SyncUnsafeCell<Idt> = SyncUnsafeCell::new(Idt::empty());
    let idt = unsafe { &mut *IDT.get() };
    idt.init();
    unsafe {
        idt.load();
    }
}

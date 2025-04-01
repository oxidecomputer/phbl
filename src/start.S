// The reset vector and early boot start for the pico host boot
// loader.
//
// This code is responsible for setting up an execution
// environment for Rust code, and nothing more.  We do the bare
// minimum reqired in assembler and leave the heavy lifting to
// Rust.

// Definitions for bits in %cr0.
CR0_PE =		1 << 0
CR0_ET =		1 << 4
CR0_WP =		1 << 16
CR0_PG =		1 << 31
CR0_MB1 =		CR0_ET

// Definitions for bits in %cr4.
CR4_PAE =		1 << 5

// Constants for the EFER MSR.
IA32_EFER_MSR =		0xc0000080
EFER_LME =		1 << 8
EFER_NX =		1 << 11

// Memory type range register related constants.
IA32_MTRR_DEF_TYPE_MSR = 0x2FF
MTRR_ENABLE =		1 << 11
MTRR_WB =		0x06

// Paging constants.
PAGE_SIZE =		4096
PG_R =			1 << 0
PG_W =			1 << 1
PG_WT =			1 << 3
PG_NC =			1 << 4
PG_HUGE =		1 << 7
PG_X =			0 << 63
PG_NX =			1 << 63

// Segmentation constants for 16, 32 and 64 bit.
GDT_NULL =		0 << 3
GDT_CODE64 =		1 << 3
GDT_CODE32 =		2 << 3
GDT_DATA32 =		3 << 3
.globl GDT_CODE64

SEG_CODE_RO =		1 << 41	// Only for code, not data segments.
SEG_DATA_RW =		1 << 41	// Only for data, not code segments.
SEG_DATA =		0 << 43
SEG_CODE =		1 << 43	// Code segments are read-only.
SEG_PRESENT =		1 << 47
SEG_LONG =		1 << 53
SEG_MUSTBE1 =		1 << 44

SEG32_DEFAULT =		1 << 54
SEG32_GRANULARITY =	1 << 55
SEG32_BASE =		0 << 16
SEG32_LIMIT =		(0xF << 48) + 0xFFFF		// 4GiB
SEG32_BOUNDS =		(SEG32_BASE + SEG32_LIMIT)	// [0..4GiB)
SEG32 =			(SEG32_DEFAULT + SEG32_GRANULARITY + SEG32_BOUNDS)

SEG16_MASK =		0xFFFF

// Stack configuration.
STACK_SIZE =		8 * PAGE_SIZE
.globl STACK_SIZE

// This is mapped to the reset vector and provides the
// first x86 instructions executed when the CPU starts.
// Architecturally, both IF and DF are defined to be
// clear after RESET, but it never hurts to be explicit,
// so we clear both and then simply jump to the 16-bit
// startup code.
.section ".reset", "a", @progbits
.globl reset
reset:
	cli
	cld
	jmp	start
	ud2
	.balign	16, 0xff

// Real execution begins here.  Load a GDT and jump to
// 32-bit protected mode.
//
// Note that since there is no firmware to set the A20
// latch, we do not have to deal with it.  Similarly,
// we do not mask out the PIC, as there is no PIC on
// Oxide machines.
.section ".start", "a", @progbits
.balign PAGE_SIZE
.code16
start:
	// Save the BIST data.
	movl	%eax, %ebp

	// Coming out of reset, caching and write-back are
	// disabled.  Clear the cache-inhibiting bits in %cr0
	// by setting the register to have only the reserved
	// bits set: in this case, only CR0_ET.
	movl	$CR0_MB1, %eax
	movl	%eax, %cr0

	// Set up a GDT.  Since we are in 16-bit real mode, the
	// GDT descriptor must be within the current segment,
	// and we know one is present because the linker put one
	// there.  We calculate its offset and load it into the
	// GDTR.
	//
	// Note that the GDT remains at the same linear address,
	// which is within the first 4GiB of address space, for
	// the lifetime of the program.  Thus, we do not bother
	// rewriting the GDTR as we move to different modes.
	movl	$gdtdesc, %ebx
	andl	$SEG16_MASK, %ebx
	lgdtl	%cs:(%bx)

	// Enable protected mode.
	movl	%cr0, %eax
	orl	$CR0_PE, %eax
	movl	%eax, %cr0

	// Jump to 32-bit code.
	ljmpl	$GDT_CODE32, $1f

.balign 64
.code32
1:
	// Set up data segmentation so that we have access to
	// the full 32-bit protected mode address space.  We
	// don't use FS or GS in the bootstrap code, so leave
	// those at their reset values (0).
	movw	$GDT_DATA32, %ax
	movw	%ax, %ds
	movw	%ax, %es
	movw	%ax, %ss

	// Enable MTRRs and set the default memory access type
	// to writeback.  Coming out of reset, all of physical
	// memory is considered UC.  Enabling MTRRs and setting
	// this to writeback enables cache control via
	// attributes in page table entries.
	// See Intel SDM vol 3A sec 11.11.2.1 for details.
	movl	$IA32_MTRR_DEF_TYPE_MSR, %ecx
	movl	$(MTRR_ENABLE | MTRR_WB), %eax
	xorl	%edx, %edx
	wrmsr

	// Enable the physical address extension in %cr4.
	movl	%cr4, %eax
	orl	$CR4_PAE, %eax
	movl	%eax, %cr4

	// Load the page table root pointer into the MMU.
	movl	$pml4, %eax
	movl	%eax, %cr3

	// Enable long mode and support for the the NX bit in
	// PTEs.
	movl	$IA32_EFER_MSR, %ecx
	movl	$(EFER_LME | EFER_NX), %eax
	xorl	%edx, %edx
	wrmsr

	// Enable paging and write-protect enforcement for the
	// kernel.  Since PAE is enabled in %cr4 and long mode
	// is enabled in the EFER MSR, the MMU uses 4 level
	// paging.
	movl	%cr0, %eax
	orl	$(CR0_PG | CR0_WP), %eax
	movl	%eax, %cr0

	// Jump to 64-bit code.
	ljmpl	$GDT_CODE64, $start64

// Define a GDT for the loader.  We provide a 64-bit code
// segment and 32-bit code and data segments.
.section ".start.rodata", "a", @progbits
.balign 64
gdt:
	// 0x0: Null segment.
	.quad	0
	// 0x8: 64-bit code segment.
	.quad	(SEG_PRESENT + SEG_CODE_RO + SEG_CODE + SEG_LONG + SEG_MUSTBE1)
	// 0x10: 32-bit code segment.
	.quad	(SEG_PRESENT + SEG_CODE_RO + SEG_CODE + SEG32 + SEG_MUSTBE1)
	// 0x18: 32-bit data segment.
	.quad	(SEG_PRESENT + SEG_DATA_RW + SEG_DATA + SEG32 + SEG_MUSTBE1)
egdt:

.skip 6
gdtdesc:
	.word	egdt - gdt - 1
	.quad	gdt

.text
.balign 64
.code64
start64:
	// Clear the segmentation registers.
	// %fs and %gs were cleared on reset, so no
	// need to clear them again.
	xorl	%eax, %eax
	movw	%ax, %ds
	movw	%ax, %es
	movw	%ax, %ss

	// Zero out the BSS.
	movq	$ebss, %rcx
	movq	$sbss, %rdi
	xorl	%eax, %eax
	subq	%rdi, %rcx
	rep; stosb

	// Set up the stack.
	movq	$stack, %rsp
	addq	$STACK_SIZE, %rsp

	// Call `init`.  This remaps the kernel, initializes the
	// UART, and sets up the IDT.  It also validates the
	// BIST data.  If init completes successfully, we call
	// `entry` with its return value, a mutable reference
	// to the system `Config`.
	movl	%ebp, %edi
	xorl	%ebp, %ebp
	call	init
	movq	%rax, %rdi
	call	entry

// Do not resuscitate.  If main ever returns, we fall
// through to this code; we also call it from panic.
.balign 64
.globl dnr
dnr:
	cli
	hlt
	jmp	dnr
	ud2

// The rodata section contains space for the early page tables.
// We leave assembler with an identity mapping for the second
// and fourth GiB of address space, which contains the loader
// and MMIO areas, respectively.  Rust code remaps everything
// almost immediately, but this way, the UART is usable in
// early boot.
.rodata
.balign PAGE_SIZE
pml4:
	.quad	pml3 + (PG_R | PG_W | PG_X)
	.space	PAGE_SIZE - 8

pml3:
	.quad	0
	.quad	(1 << 30) + (PG_HUGE | PG_R | PG_W | PG_X)
	.quad	0
	.quad	(3 << 30) + (PG_HUGE | PG_R | PG_W | PG_NX | PG_NC | PG_WT)
	.space	PAGE_SIZE - 4 * 8

// The only data we define in the BSS in assembler is
// the Rust stack.
.bss
.balign PAGE_SIZE
.globl stack
stack:
	.space	STACK_SIZE

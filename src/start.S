// The reset vector and early boot start for the pico host boot
// loader.
//
// This code is responsible for setting up an execution
// environment for Rust code, and nothing more:
//
// Copyright 2022, Oxide Computer Company

// Definitions for bits in %cr0.
CR0_PE =		1 << 0
CR0_PG =		1 << 31

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
PG_W =			1 << 2
PG_NC =			1 << 4
PG_HUGE =		1 << 7
PG_X =			0 << 63
PG_NX =			1 << 63

// Segmentation constants for 16, 32 and 64 bit.
GDT_NULL =		0 << 3
GDT_CODE64 =		1 << 3
GDT_CODE32 =		2 << 3
GDT_DATA32 =		3 << 3

SEG_READ =		1 << 41
SEG_WRITE =		1 << 42
SEG_DATA =		0 << 43
SEG_CODE =		1 << 43
SEG_PRESENT =		1 << 47
SEG_LONG =		1 << 53
SEG_MUSTBE1 =		1 << 44

SEG32_DEFAULT =		1 << 54
SEG32_GRANULARITY =	1 << 55
SEG32_LIMIT =		(0xF << 48) + 0xFFFF
SEG32 =			(SEG32_DEFAULT + SEG32_GRANULARITY + SEG32_LIMIT)

SEG16_MASK =		0xFFFF

// Debug-related data
DEBUG_PORT =		0x80
DEBUG_HELLO =		0x1DE
DEBUG_HALTED =		0xFFFFDEAD

// Stack configuration.
.globl STACK_SIZE
STACK_SIZE =		8 * PAGE_SIZE

// This is mapped to the reset vector and is it first
// x86 instruction executed when the CPU comes out of
// reset.  It simply jumps to the 16-bit startup code.
.section ".reset", "ax", @progbits
.globl reset
reset:
	jmp	start

// Real execution begins here.  Clear interrupts,
// the direction flag, load a GDT and jump to 32-bit
// protected mode.
//
// Note that since there is no firmware to set the A20
// latch, we do not have to deal with it.  Similarly,
// we do not mask out the PIC, as there is no PIC on an
// Oxide machine and this code is specific to Oxide
// machines.
.section ".start", "ax", @progbits
.balign PAGE_SIZE
.code16
start:
	cli
	cld

	// Save the BIST data.
	movl	%eax, %ebp

	// Set up a GDT.  Since we are in 16-bit real mode, the
	// GDT descriptor must be within the current segment.
	// We know that we have such a GDT descriptor because we
	// force the linker to put one in the current segment.
	// But we choose not to rely on exact knowledge of where
	// in the segment the linker put it.  Instead, we know
	// that the reset vector is exactly 16 bytes before the
	// end of the segment, so we use that to calculate the
	// segment base address and thus the segment-relative
	// descriptor offset.
	movl	$gdtdesc, %ebx
	movl	$reset, %eax
	subl	$(64 * 1024 - 16), %eax
	subl	%eax, %ebx
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
	// Set up data segmentation.  We now have access to the
	// full 32-bit linear address space accessible from
	// protected mode.
	movw	$GDT_DATA32, %ax
	movw	%ax, %ds
	movw	%ax, %es
	movw	%ax, %fs
	movw	%ax, %gs
	movw	%ax, %ss

	// Enable writeback as the default memory type via the
	// MTRRs.
	movl	$IA32_MTRR_DEF_TYPE_MSR, %ecx
	movl	$(MTRR_ENABLE | MTRR_WB), %eax
	xorl	%edx, %edx
	wrmsr

	// Enable long mode and the NX bit.
	movl	$IA32_EFER_MSR, %ecx
	movl	$(EFER_LME | EFER_NX), %eax
	xorl	%edx, %edx
	wrmsr

	// Enable the physical address extension in %cr4.
	movl	%cr4, %eax
	orl	$CR4_PAE, %eax
	movl	%eax, %cr4

	// Set the page table root pointer.
	movl	$pml4, %eax
	movl	%eax, %cr3

	// Enable paging.
	movl	%cr0, %eax
	orl	$CR0_PG, %eax
	movl	%eax, %cr0

	// Jump to 64-bit code.
	ljmpl	$GDT_CODE64, $1f

.balign 64
.code64
1:
	// Clear the segmentation registers.
	xorl	%eax, %eax
	movw	%ax, %ds
	movw	%ax, %es
	movw	%ax, %fs
	movw	%ax, %gs
	movw	%ax, %ss

	// Zero out the BSS.
	movq	$end, %rcx
	movq	$edata, %rdi
	xorl	%eax, %eax
	subq	%rdi, %rcx
	shrq	$3, %rcx
	rep; stosq

	// Set up the stack.
	movq	$stack, %rsp
	addq	$STACK_SIZE, %rsp

	// Call init.  This remaps the kernel, initializes the
	// UART, and sets up the IDT.  It also validates the
	// BIST data.  If init completes successfully, we call
	// main.
	movl	%ebp, %edi
	xorl	%ebp, %ebp
	call	init
	call	entry

// Do not resuscitate.  If main ever returns, we fall
// through to this code; we also call it from panic.
.balign 64
.globl dnr
dnr:
	movl	$DEBUG_HALTED, %eax
	outl	%eax, $DEBUG_PORT

1:	cli
	hlt
	jmp	1b
	ud2

// Define a GDT for the loader.  We provide a 64-bit code
// segment and 32-bit code and data segments.
.section ".start.rodata", "a", @progbits
.balign 64
gdt:
	// Null segment
	.quad	0
	// 64-bit code segment.
	.quad	(SEG_PRESENT + SEG_READ + SEG_CODE + SEG_LONG + SEG_MUSTBE1)
	// 32-bit code segment.
	.quad	(SEG_PRESENT + SEG_READ + SEG_CODE + SEG32 + SEG_MUSTBE1)
	// 32-bit data segment.
	.quad	(SEG_PRESENT + SEG_WRITE + SEG32 + SEG_MUSTBE1)
egdt:

.skip 6
gdtdesc:
	.word	egdt - gdt - 1
	.quad	gdt

// The data section contains space for the early page tables.
// We leave assembler with an identity mapping for the second
// and third GiB of address space, which contains the loader
// and MMIO areas, respectively.  Rust code remaps almost
// immediately, but this way, the UART is usable in early boot.
.rodata
.balign PAGE_SIZE
pml4:
	.quad	pml3 + (PG_R | PG_W | PG_X)
	.space	PAGE_SIZE - 8

pml3:
	.quad	0
	.quad	(1 << 30) + (PG_HUGE | PG_W | PG_X)
	.quad	(2 << 30) + (PG_HUGE | PG_W | PG_X | PG_NC)
	.space	PAGE_SIZE - 3 * 8

// The only data we define in the BSS in assembler is
// the Rust stack.
.bss
.balign PAGE_SIZE
.globl stack
stack:
	.space	STACK_SIZE

/*
 * Linker script for the Pico Host Boot Loader
 */
ENTRY(reset);

HIDDEN(bootblock = 0x000000007ffef000);
HIDDEN(resetaddr = 0x000000007ffefff0);

SECTIONS {
	.start bootblock : {
		FILL(0xffffffff);
		*(.start)
	} :srodata
	.start.rodata ALIGN(64) : {
		*(.start.rodata)
	} :srodata
	.reset resetaddr : {
		FILL(0xffffffff);
		*(.reset)
		__eloader = ALIGN(65536);
	} :srodata

	.data ((ADDR(.start) - datasize) & ~0xFFF) : {
		FILL(0xffffffff);
		*(.data*)
		edata = ALIGN(4096);
	} :data

	.rodata ((ADDR(.data) - rodatasize) & ~0xFFF): {
		FILL(0xffffffff);
		*(.rodata*)
		erodata = ALIGN(4096);
	} :rodata

	.text ((ADDR(.rodata) - textsize) & ~0xFFF) : {
		__sloader = .;
		FILL(0xffffffff);
		*(.text*)
		etext = ALIGN(4096);
	} :text

	.bss ((ADDR(.text) - bsssize) & ~0xFFF) (NOLOAD) : {
		sbss = .;
		*(.bss* COMMON)
		ebss = ALIGN(4096);
	}

	textsize = SIZEOF(.text);
	rodatasize = SIZEOF(.rodata);
	datasize = SIZEOF(.data);
	bsssize = SIZEOF(.bss);

	_BL_SPACE = __eloader - __sloader;

	/DISCARD/ : {
		*(.got* .comment* .note* .eh_frame*)
	}
}

PHDRS {
	text	PT_LOAD;
	rodata	PT_LOAD;
	data	PT_LOAD;
	srodata	PT_LOAD;
}

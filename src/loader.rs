// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! Parses and loads a binary image, represented as a slice of
//! bytes, into its executable form in RAM.

extern crate alloc;

use crate::mem;
use crate::mmu::LoaderPageTable;
use crate::Result;
use alloc::vec::Vec;
use goblin::container::{Container, Ctx, Endian};
use goblin::elf::program_header::PT_LOAD;
use goblin::elf::ProgramHeader;
use goblin::elf::{self, Elf};

type Thunk = unsafe extern "C" fn(ramdisk_addr: u64, ramdisk_len: usize);

/// Loads an executable image contained in the given byte slice,
/// creating virtual mappings as required.  Returns the image's
/// ELF entry point on success.
pub(crate) fn load(
    page_table: &mut LoaderPageTable,
    bytes: &[u8],
) -> Result<Thunk> {
    let elf = parse_elf(bytes)?;
    for section in elf.program_headers.iter().filter(|&h| h.p_type == PT_LOAD) {
        let file_range = section.file_range();
        if bytes.len() < file_range.end {
            return Err("load: truncated executable");
        }
        load_segment(page_table, section, &bytes[file_range])?;
    }
    Ok(unsafe { core::mem::transmute::<_, Thunk>(elf.entry) })
}

/// Parses the ELF executable contained in the given byte slice.
fn parse_elf(bytes: &[u8]) -> Result<Elf> {
    let header = parse_header(bytes)?;
    let mut elf = Elf::lazy_parse(header).map_err(|_| "parsed ELF binary")?;
    elf.program_headers = parse_program_headers(bytes, header)?;
    Ok(elf)
}

/// Parses and validates the ELF header from the given byte
/// slice.  Note that much of the heavy lifting of validating
/// the ELF header is done by the parsing library.
fn parse_header(bytes: &[u8]) -> Result<elf::Header> {
    let binary = Elf::parse_header(bytes).map_err(|_| "parsed ELF header")?;
    if binary.e_machine != elf::header::EM_X86_64 {
        return Err("ELF: incorrect machine architecture");
    }
    let container = binary.container().map_err(|_| "ELF: bad class")?;
    if container != Container::Big {
        return Err("ELF: object file is not 64-bit");
    }
    let endian = binary.endianness().map_err(|_| "ELF: bad endianness")?;
    if endian != Endian::Little {
        return Err("ELF: object file is not little-endian");
    }
    if binary.e_type != elf::header::ET_EXEC {
        return Err("ELF: object file is not executable");
    }
    if binary.e_entry == 0 {
        return Err("ELF: binary has nil entry point");
    }
    if binary.e_ident[elf::header::EI_VERSION] != elf::header::EV_CURRENT
        || binary.e_version != elf::header::EV_CURRENT.into()
    {
        return Err("ELF: bad ELF version number");
    }
    // Apparently, illumos uses the 'ELFOSABI_SOLARIS' ABI type
    // for the kernel.  Ignore this for now.
    // if binary.e_ident[elf::header::EI_OSABI] != elf::header::ELFOSABI_NONE {
    //     return Err("ELF: bad image ABI (is not NONE)");
    // }
    Ok(binary)
}

/// Parses the ELF program headers in the contained given image
/// and header.  Separated from parsing the rest of the image
/// as we want to avoid excessive allocations for things that we
/// do not use, such as the symbol and strings tables.
fn parse_program_headers(
    bytes: &[u8],
    header: elf::Header,
) -> Result<Vec<ProgramHeader>> {
    let container = header.container().map_err(|_| "ELF: Bad container")?;
    let endian = header.endianness().map_err(|_| "ELF: Bad endianness")?;
    let ctx = Ctx::new(container, endian);
    ProgramHeader::parse(
        bytes,
        header.e_phoff as usize,
        header.e_phnum as usize,
        ctx,
    )
    .map_err(|_| "cannot parse ELF program headers")
}

/// Loads the given ELF segment, creating virtual mappings for
/// it as required.
fn load_segment(
    page_table: &mut LoaderPageTable,
    section: &ProgramHeader,
    bytes: &[u8],
) -> Result<()> {
    let pa = section.p_paddr as u64;
    if pa % mem::P4KA::ALIGN != 0 {
        return Err("Program section is not physically 4KiB aligned");
    }
    let vm = section.vm_range();
    if vm.contains(&mem::LOW_CANON_SUP) || vm.contains(&mem::HI_CANON_INF) {
        return Err("Program section is not canonical");
    }
    if vm.start % mem::V4KA::ALIGN != 0 {
        return Err("Program section not virtually 4KiB aligned");
    }
    if vm.end <= vm.start {
        return Err("Program section ends before start or is empty");
    }
    let start = mem::V4KA::new(vm.start);
    let end = mem::V4KA::new(round_up_2m(vm.end));
    let region = start..end;
    let pa = mem::P4KA::new(pa);
    {
        let dst = unsafe {
            page_table
                .map_region(region.clone(), mem::Attrs::new_data(), pa)
                .expect("mapped region {region:#x?} read-write");
            let p = page_table.try_with_addr(start.addr()).unwrap();
            let len = end.addr() - start.addr();
            core::ptr::write_bytes(p, 0, len);
            core::slice::from_raw_parts_mut(p, len)
        };
        let len = usize::min(bytes.len(), dst.len());
        if len > 0 {
            dst[..len].copy_from_slice(&bytes[..len]);
        }
    }
    let attrs = mem::Attrs::new_kernel(
        section.is_read(),
        section.is_write(),
        section.is_executable(),
    );
    unsafe {
        page_table
            .map_region(region, attrs, pa)
            .expect("remapped region {region:#x?} with attrs {attrs:?}");
    }
    Ok(())
}

/// Aligns the given address up to the next higher 2MiB
/// boundary.
fn round_up_2m(va: usize) -> usize {
    const MASK: usize = (1 << 21) - 1;
    va.wrapping_add(MASK) & !MASK
}

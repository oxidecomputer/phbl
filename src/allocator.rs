// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//
// This file incorporates work covered by the following copyright and
// permission notice:
//
//   Copyright 2021  The Hypatia Authors
//   All rights reserved
//
//   Use of this source code is governed by an MIT-style
//   license that can be found in the LICENSE file or at
//   https://opensource.org/licenses/MIT.

extern crate alloc;

use alloc::alloc::{AllocError, Allocator, GlobalAlloc, Layout};
use core::cell::UnsafeCell;
use core::ops::Range;
use core::ptr;
use core::sync::atomic::{AtomicUsize, Ordering};

/// A simple bump allocator for use as a global allocator
/// (for Goblin) as well as implementing the specific
/// allocator interface (for page tables).
#[repr(C, align(4096))]
pub(crate) struct BumpAlloc<const SIZE: usize> {
    heap: UnsafeCell<[u8; SIZE]>,
    offset: AtomicUsize,
}

impl<const SIZE: usize> BumpAlloc<SIZE> {
    pub(crate) const fn new(buffer: [u8; SIZE]) -> BumpAlloc<SIZE> {
        BumpAlloc { heap: UnsafeCell::new(buffer), offset: AtomicUsize::new(0) }
    }

    /// Allocates a region of memory of the given alignment and
    /// size.
    pub(crate) fn alloc_bytes(
        &self,
        align: usize,
        size: usize,
    ) -> Option<&mut [u8]> {
        let heap = self.base();
        let mut pos = 0;
        self.offset
            .fetch_update(Ordering::SeqCst, Ordering::SeqCst, |offset| {
                let current = unsafe { heap.add(offset) };
                let adjust = current.align_offset(align);
                pos = offset.checked_add(adjust).expect("alignment overflow");
                let next = pos.checked_add(size).expect("size overflow");
                if next > SIZE {
                    return None;
                }
                Some(next)
            })
            .ok()?;
        let ptr = unsafe { heap.add(pos) };
        Some(unsafe { core::slice::from_raw_parts_mut(ptr, size) })
    }

    /// Returns a raw pointer to the heap.  Useful for
    /// reconstructing provenance.
    pub(crate) fn base(&self) -> *mut u8 {
        self.heap.get() as *mut u8
    }

    /// Returns the range of addresses in the heap, for
    /// validating that an integral value lies within
    /// the heap.
    pub(crate) fn addr_range(&self) -> Range<usize> {
        let start = self.base().addr();
        let end = start + SIZE;
        start..end
    }
}

/// By implementing the Allocator interface, we can allocate in
/// a static region, e.g., such as that owned by the page
/// allocator used for creating page tables.
unsafe impl<const SIZE: usize> Allocator for BumpAlloc<SIZE> {
    fn allocate(
        &self,
        layout: Layout,
    ) -> Result<ptr::NonNull<[u8]>, AllocError> {
        let p = self
            .alloc_bytes(layout.align(), layout.size())
            .ok_or(AllocError)?;
        Ok(p.into())
    }
    unsafe fn deallocate(&self, _ptr: ptr::NonNull<u8>, _layout: Layout) {}
}

/// By implementing GlobalAlloc, we can use an ELF parsing
/// library that uses Vec etc.
unsafe impl<const SIZE: usize> GlobalAlloc for BumpAlloc<SIZE> {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        self.alloc_bytes(layout.align(), layout.size())
            .map_or(ptr::null_mut(), |p| p.as_mut_ptr())
    }
    unsafe fn dealloc(&self, _ptr: *mut u8, _layout: Layout) {}
}

#[cfg(test)]
mod bump_tests {
    use super::BumpAlloc;

    #[test]
    fn simple_alloc() {
        let allocator = BumpAlloc::new([0; 128]);

        let a = allocator.alloc_bytes(4, 4).unwrap().as_ptr();
        let b = allocator.alloc_bytes(4, 4).unwrap().as_ptr();
        assert_eq!(a.addr() + 4, b.addr());
    }
}

#[cfg(not(any(test, clippy)))]
mod global {
    use super::BumpAlloc;

    const HEAP_SIZE: usize = 2 * 1024 * 1024;

    #[global_allocator]
    static mut BUMP_ALLOCATOR: BumpAlloc<HEAP_SIZE> =
        BumpAlloc::new([0; HEAP_SIZE]);
}

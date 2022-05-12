// Copyright 2021  The Hypatia Authors
// All rights reserved
//
// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/MIT.

extern crate alloc;

use alloc::alloc::{AllocError, Allocator, GlobalAlloc, Layout};
use core::cell::UnsafeCell;
use core::ptr;
use core::sync::atomic::{AtomicUsize, Ordering};

/// A simple bump allocator for use as a global allocator
/// (for Goblin) as well as implementing the specific
/// allocator interface (for page tables).
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
        let heap = self.heap.get() as *mut u8;
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
        assert_eq!(a as usize + 4, b as usize);
    }
}

#[cfg(not(any(test, feature = "cargo-clippy")))]
mod global {
    use super::{alloc, BumpAlloc};

    const HEAP_SIZE: usize = 2 * 1024 * 1024;

    #[global_allocator]
    static mut BUMP_ALLOCATOR: BumpAlloc<HEAP_SIZE> =
        BumpAlloc::new([0; HEAP_SIZE]);

    #[alloc_error_handler]
    pub fn oom(layout: alloc::alloc::Layout) -> ! {
        panic!("Early allocation failed on size {}", layout.size());
    }
}

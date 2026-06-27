use std::alloc::GlobalAlloc;

use sart::salloc;

pub struct SaAllocator;

unsafe impl GlobalAlloc for SaAllocator {
  unsafe fn alloc(&self, layout: std::alloc::Layout) -> *mut u8 {
    unsafe { salloc::aligned_malloc(layout.size(), layout.align()) as _ }
  }

  unsafe fn alloc_zeroed(&self, layout: std::alloc::Layout) -> *mut u8 {
    unsafe { salloc::aligned_zalloc(layout.size(), layout.align()) as _ }
  }

  unsafe fn dealloc(&self, ptr: *mut u8, _: std::alloc::Layout) {
    unsafe { salloc::aligned_free(ptr as _) as _ }
  }

  unsafe fn realloc(&self, ptr: *mut u8, layout: std::alloc::Layout, new_size: usize) -> *mut u8 {
    unsafe { salloc::aligned_realloc(ptr as _, new_size, layout.align()) as _ }
  }
}

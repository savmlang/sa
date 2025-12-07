use std::{cell::UnsafeCell, mem::zeroed};

use sart::map::HeapStructure;

thread_local! {
  pub static SYNC_HEAP: UnsafeCell<SyncHeapMapStore> = UnsafeCell::new(SyncHeapMapStore { heaps: Box::new(unsafe { zeroed() }), points: Vec::with_capacity(5), length: 0 });
}

pub type OwnedHeap = [HeapStructure; 256];

pub struct SyncHeapMapStore {
  // We allocate `21` so that we have 20 to share + 1 main VM
  pub heaps: Box<[OwnedHeap; 21]>,
  pub points: Vec<*mut HeapStructure>,
  pub length: usize,
}

impl SyncHeapMapStore {
  /// Returns the 1st pointer to a new heap struct
  /// This is an array but for convenience reasons we give you the 1st pointer only
  pub fn get(&mut self) -> *mut HeapStructure {
    unsafe {
      let length = &mut self.length;

      // If the next pointer is larger the total allocated size
      // We must allocate
      if *length >= 21 {
        *length += 1;

        let heap: Box<OwnedHeap> = Box::new(zeroed());

        let pointer = Box::into_raw(heap) as *mut HeapStructure;

        self.points.push(pointer);

        return pointer;
      }

      // Assuming length as the next index
      let out = self.heaps.get_unchecked_mut(*length);

      *length += 1;

      out as *mut _ as *mut HeapStructure
    }
  }

  /// You must call this in exact strict deallocation order
  ///
  /// that means that
  /// if you get
  /// - ..
  /// - heap 21
  /// - heap 22
  /// - heap 23
  ///
  /// you must call this like this
  /// - heap 23
  /// - heap 22
  /// - heap 21
  /// ...
  pub fn collect(&mut self) -> *mut HeapStructure {
    unsafe {
      let length = &mut self.length;

      #[cfg(debug_assertions)]
      if *length == 0 {
        panic!("Please use the API Correctly. E_CONDITION_01_CALLED_WITH_LENGTH_0");
      }

      if *length >= 22 {
        *length -= 1;

        let heap = self.points.pop().unwrap_unchecked();
        drop(Box::from_raw(heap as *mut OwnedHeap));

        if *length > 21 {
          return *self.points.last().unwrap_unchecked() as _;
        }
      } else {
        *length -= 1;
      }

      #[cfg(debug_assertions)]
      if *length == 0 {
        panic!("Please use the API Correctly. E_CONDITION_01_CALLED_WITH_LENGTH_1");
      }

      // We so get the previous heap data right?
      self.heaps.get_unchecked_mut(*length - 1) as *mut _ as _
    }
  }
}

use std::{
  cell::UnsafeCell,
  collections::HashMap,
  hash::Hash,
  marker::PhantomData,
  num::NonZeroUsize,
  rc::Rc,
  sync::{
    Arc,
    atomic::{AtomicUsize, Ordering},
  },
};

use dashmap::DashMap;

pub mod llir;
pub mod mir;

#[repr(align(64))]
pub struct SingleThreadedStringStore {
  data: UnsafeCell<Vec<Rc<str>>>,
  sets: UnsafeCell<HashMap<Rc<str>, NonZeroUsize, rapidhash::fast::RandomState>>,
}

impl SingleThreadedStringStore {
  pub fn new() -> Self {
    Self {
      data: UnsafeCell::new(Default::default()),
      sets: UnsafeCell::new(Default::default()),
    }
  }
}

impl StringStore for SingleThreadedStringStore {
  type T<'a>
    = Rc<str>
  where
    Self: 'a;

  fn matchval<'a, Data: AsRef<str>>(&'a self, data: Data) -> StringRef<'a, Self> {
    let sdata = data.as_ref();

    let hmap = unsafe { &*self.sets.get() };

    let Some(&data) = hmap.get(sdata) else {
      let hmap = unsafe { &mut *self.sets.get() };
      let vector = unsafe { &mut *self.data.get() };

      let rcd: Rc<str> = Rc::from(sdata);

      vector.push(rcd.clone());

      let newid = unsafe { NonZeroUsize::new_unchecked(vector.len()) };
      _ = hmap.insert(rcd, newid);

      return StringRef {
        _inner: newid,
        _parent: PhantomData,
      };
    };

    return StringRef {
      _inner: data,
      _parent: PhantomData,
    };
  }

  fn resolve<'a>(&'a self, sref: StringRef<'a, Self>) -> Self::T<'a> {
    unsafe {
      (&*self.data.get())
        .get_unchecked(sref._inner.get() - 1)
        .clone()
    }
  }
}

#[repr(align(64))]
pub struct MultiThreadedStringStore {
  countgen: AtomicUsize,
  vect: DashMap<NonZeroUsize, Arc<str>, rapidhash::fast::RandomState>,
  sets: DashMap<Arc<str>, NonZeroUsize, rapidhash::fast::RandomState>,
}

impl MultiThreadedStringStore {
  pub fn new() -> Self {
    Self {
      countgen: AtomicUsize::new(0),
      sets: Default::default(),
      vect: Default::default(),
    }
  }
}

impl StringStore for MultiThreadedStringStore {
  type T<'a>
    = Arc<str>
  where
    Self: 'a;

  fn resolve<'a>(&'a self, sref: StringRef<'a, Self>) -> Self::T<'a> {
    self
      .vect
      .get(&sref._inner)
      .expect("Since a StringRef was earlier made - this should not be empty")
      .clone()
  }

  fn matchval<'a, Data: AsRef<str>>(&'a self, data: Data) -> StringRef<'a, Self> {
    let sdata = data.as_ref();

    if let Some(dt) = self.sets.get(sdata) {
      return StringRef {
        _inner: *dt,
        _parent: PhantomData,
      };
    }

    let value: Arc<str> = Arc::from(sdata);
    let new_idx = self.sets.entry(value.clone()).or_insert_with(|| {
      let raw_idx = self.countgen.fetch_add(1, Ordering::AcqRel) + 1;
      let idx = unsafe { NonZeroUsize::new_unchecked(raw_idx) };

      self.vect.insert(idx, value);
      idx
    });

    StringRef {
      _inner: *new_idx,
      _parent: PhantomData,
    }
  }
}

pub trait StringStore {
  type T<'a>: AsRef<str> + 'a
  where
    Self: 'a;

  fn matchval<'a, Data: AsRef<str>>(&'a self, data: Data) -> StringRef<'a, Self>;

  fn resolve<'a>(&'a self, sref: StringRef<'a, Self>) -> Self::T<'a>;
}

#[derive(Debug)]
pub struct StringRef<'a, T: StringStore + ?Sized> {
  _inner: NonZeroUsize,
  _parent: PhantomData<&'a T>,
}

impl<'a, T: StringStore + ?Sized> Hash for StringRef<'a, T> {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self._inner.hash(state);
  }
}

impl<'a, T: StringStore + ?Sized> PartialEq for StringRef<'a, T> {
  fn eq(&self, other: &Self) -> bool {
    self._inner.eq(&other._inner)
  }
}

impl<'a, T: StringStore + ?Sized> Eq for StringRef<'a, T> {}

impl<'a, T: StringStore + ?Sized> Clone for StringRef<'a, T> {
  fn clone(&self) -> Self {
    Self {
      _inner: self._inner,
      _parent: PhantomData,
    }
  }
}

impl<'a, T: StringStore + ?Sized> Copy for StringRef<'a, T> {}

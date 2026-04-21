use super::*;

pub type Addr = Rc<RefCell<Term>>;

// Heap-allocated array (as backing store), that is dynamic array itself or is
// refered / shared by non-empty Slice or Ptr.
pub type Buffer = Rc<RefCell<Vec<Term>>>;

// Whole or Partial view of Buffer.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Slice {
    Empty,
    Some { arr: Buffer, beg: usize, end: usize },
}

// Pointer to an element in a Buffer.
pub struct Ptr { arr: Buffer, idx: usize }

pub trait ArrayLike {
    fn len(&self) -> usize;
    fn capacity(&self) -> usize;
    fn slice(&self, beg: usize, end: usize) -> Slice;
    fn ptr(&self, idx: usize) -> Ptr;
}

impl ArrayLike for Buffer {
    fn len(&self) -> usize {
        self.borrow().len()
    }
    fn capacity(&self) -> usize {
        self.borrow().capacity()
    }
    fn slice(&self, beg: usize, end: usize) -> Slice {
        assert!(beg <= end && end <= self.len());
        if beg == end {
            Slice::Empty
        }
        else {
            Slice::Some { arr: self.clone(), beg, end }
        }
    }
    fn ptr(&self, idx: usize) -> Ptr {
        assert!(idx <= self.len());
        Ptr { arr: self.clone(), idx }
    }
}

impl ArrayLike for Slice {
    fn len(&self) -> usize {
        match self {
            Slice::Empty => 0,
            Slice::Some { arr: _, beg, end } => end - beg,
        }
    }
    fn capacity(&self) -> usize {
        self.len()
    }
    fn slice(&self, beg: usize, end: usize) -> Slice {
        assert!(beg <= end && end <= self.len());
        if beg == end {
            Slice::Empty
        }
        else if let Slice::Some { arr, beg: start, end: stop } = self {
            // only permitted to narrow the range of the slice.
            let beg = start + beg;
            let end = start + end;
            assert!(end <= *stop);
            Slice::Some { arr: arr.clone(), beg, end }
        }
        else {
            panic!();
        }
    }
    fn ptr(&self, idx: usize) -> Ptr {
        assert!(idx <= self.len());
        match self {
            Slice::Empty => {
                panic!()
            }
            Slice::Some { arr, beg, end: _ } => {
                Ptr { arr: arr.clone(), idx: beg + idx }
            }
        }
    }
}

pub fn alloc_reserved() -> Addr {
    let dummy = Term::Val(Value::Unit);
    alloc(dummy)
}

pub fn alloc(t: Term) -> Addr {
    Rc::new(RefCell::new(t))
}

pub fn load(a: Addr) -> Term {
    a.borrow().clone()
}

pub fn store(a: Addr, t: &Term) {
    *a.borrow_mut() = t.clone()
}

pub fn array(v: Vec<Term>) -> Buffer {
    Rc::new(RefCell::new(v))
}

/// Get the element at read pointer `p`.
pub fn get(p: &Ptr) -> Term {
    p.arr.borrow()[p.idx].clone()
}

/// Replace the element at write pointer `p` with `t`.
pub fn set(p: &Ptr, t: Term) {
    p.arr.borrow_mut()[p.idx] = t
}

/// Insert `t` at write pointer `p`.
pub fn insert(p: &Ptr, t: Term) {
    let v = &mut p.arr.borrow_mut();
    v.insert(p.idx, t);
}

/// Construct immutable array from slice `s`.
pub fn extract(s: &Slice) -> Buffer {
    match s {
        Slice::Empty => array(vec![]),
        Slice::Some { arr, beg, end } => {
            let s = &arr.borrow()[*beg..*end];
            array(s.to_vec())
        }
    }
}

/// Remove elements of mutable slice `s`.
pub fn remove(s: &Slice) {
    if let Slice::Some { arr, beg, end } = s {
        let v = &mut arr.borrow_mut();
        v.drain(*beg..*end);
    }
}

// In-place memmove for `[T]` where `T: Clone`
fn memmove_clone<T: Clone>(v: &mut [T], src: usize, dst: usize, count: usize) {
    assert!(src != dst && 0 < count);

    if dst < src {
        for i in 0..count {
            v[dst + i] = v[src + i].clone();
        }
    } else {
        for i in (0..count).rev() {
            v[dst + i] = v[src + i].clone();
        }
    }
}

/// Insert elements in `s` at write pointer `p`.
/// The backing arrays for `p` and `s` must not be the same.
pub fn extend(p: &Ptr, s: &Slice) {
    if let Slice::Some { arr, beg, end } = s {
        assert!(!Rc::ptr_eq(&p.arr, arr));

        let src = &arr.borrow()[*beg..*end];
        let mut dst = p.arr.borrow_mut();

        let n = src.len();
        let old_len = dst.len();

        // shift right: resize + (in-place) memmove
        dst.resize(old_len + n, Term::Val(Value::Unit));
        memmove_clone(&mut dst, p.idx, p.idx + n, old_len - p.idx);
        // memcpy
        dst[p.idx..p.idx+n].clone_from_slice(src);
    }
}

/// Copy `s[..]` to `p.arr[p.idx..(p.idx + s.end - s.beg)]`.
/// The backing arrays of `p` and `s` may be the same or different.
/// If they are the same, the semantics of an in-place `memmove` apply.
pub fn copy(p: &Ptr, s: &Slice) {
    if let Slice::Some { arr, beg, end } = s {
        if Rc::ptr_eq(&p.arr, arr) {
            let mut dst = p.arr.borrow_mut();
            memmove_clone(&mut dst, *beg, p.idx, *end - *beg);
        }
        else {
            let src = &arr.borrow()[*beg..*end];
            let mut dst = p.arr.borrow_mut();
            let dst_range = p.idx .. p.idx + src.len();
            dst[dst_range].clone_from_slice(src);
        }
    }
}

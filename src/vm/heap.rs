use super::*;

pub type Addr = Rc<RefCell<Term>>;

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

/// Construct dynamic array.
pub fn array<T>(v: Vec<T>) -> Buf<T> {
    Rc::new(RefCell::new(v))
}

// === for arrays ===

// Heap-allocated array (as backing store), that is dynamic array itself or is
// refered / shared by non-empty Slice or Ptr.
pub type Buf<T> = Rc<RefCell<Vec<T>>>;

// Whole or Partial view of Buffer.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Slice<T> {
    Empty,
    Some { arr: Buf<T>, beg: usize, end: usize },
}

// Pointer to an element in a Buffer.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ptr<T> { arr: Buf<T>, idx: usize }

pub trait ArrayLike {
    type Elem;
    fn len(&self) -> usize;
    fn capacity(&self) -> usize;
    fn slice(&self, beg: usize, end: usize) -> Slice<Self::Elem>;
    fn ptr(&self, idx: usize) -> Ptr<Self::Elem>;
}

impl <T> ArrayLike for Buf<T> {
    type Elem = T;
    fn len(&self) -> usize {
        self.borrow().len()
    }
    fn capacity(&self) -> usize {
        self.borrow().capacity()
    }
    fn slice(&self, beg: usize, end: usize) -> Slice<T> {
        assert!(beg <= end && end <= self.len());
        if beg == end {
            Slice::Empty
        }
        else {
            Slice::Some { arr: self.clone(), beg, end }
        }
    }
    fn ptr(&self, idx: usize) -> Ptr<T> {
        assert!(idx <= self.len());
        Ptr { arr: self.clone(), idx }
    }
}

impl <T> ArrayLike for Slice<T> {
    type Elem = T;
    fn len(&self) -> usize {
        match self {
            Slice::Empty => 0,
            Slice::Some { arr: _, beg, end } => end - beg,
        }
    }
    fn capacity(&self) -> usize {
        self.len()
    }
    fn slice(&self, beg: usize, end: usize) -> Slice<T> {
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
    fn ptr(&self, idx: usize) -> Ptr<T> {
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

/// Builtin::Slice
pub fn slice<T: Clone>(s: Slice<T>, i: i64, j: i64) -> Result<Slice<T>, RuntimeError> {
    if !(0 <= i && i <= j && j <= s.len() as i64) {
        return Err(RuntimeError::IndexOutOfBounds);
    }
    Ok(s.slice(i as usize, j as usize))
}

/// Builtin::Push
pub fn push<T: Clone>(s: Slice<T>, t: T) -> Slice<T> {
    let buf = thaw(s);          // thaw!
    buf_push(&buf, t);          // push!
    freeze(buf)                 // freeze!
}

/// Index access
pub fn extract_1<T: Clone>(s: &Slice<T>, index: i64) -> Result<T, RuntimeError> {
    match s {
        Slice::Empty => {
            Err(RuntimeError::IndexOutOfBounds)
        }
        Slice::Some { arr, beg, end } => {
            if index < 0 || end - beg <= index as usize {
                Err(RuntimeError::IndexOutOfBounds)
            }
            else {
                Ok(arr.borrow()[beg + index as usize].clone())
            }
        }
    }
}

/// Construct immutable array from slice `s`.
pub fn extract<T: Clone>(s: &Slice<T>) -> Buf<T> {
    match s {
        Slice::Empty => array(vec![]),
        Slice::Some { arr, beg, end } => {
            let s = &arr.borrow()[*beg..*end];
            array(s.to_vec())
        }
    }
}

/// Consumes the immutable array and converts it to a dynamic array.
pub fn thaw<T: Clone>(s: Slice<T>) -> Buf<T> {
    match s {
        Slice::Some { arr, beg, end }
        if beg == 0 && end == arr.len()
            // => Rc::clone(&arr),
            => arr,
        _
            => {
                extract(&s)
            },
    }
}

/// Consumes the dynamic array and converts it to an immutable array.
pub fn freeze<T>(b: Buf<T>) -> Slice<T> {
    if b.len() == 0 {
        Slice::Empty
    }
    else {
        let len = b.len();
        Slice::Some { arr: b, beg: 0, end: len }
    }
}

/// Insert `t` at the end of buffer `b`.
pub fn buf_push<T: Clone>(b: &Buf<T>, t: T) {
    // insert(&b.ptr(b.len()), t);
    b.borrow_mut().push(t)
}


// -------------------------------------------------------------
// === [unused][experimental] in-place procedures ===

/// Replace the element at write pointer `p` with `t`.
pub fn replace_1<T:>(p: &Ptr<T>, t: T) {
    p.arr.borrow_mut()[p.idx] = t
}

/// Insert `t` at write pointer `p`.
pub fn insert_1<T>(p: &Ptr<T>, t: T) {
    let v = &mut p.arr.borrow_mut();
    v.insert(p.idx, t);
}

/// Remove the element at write pointer `p`.
pub fn remove_1<T>(p: &Ptr<T>) {
    let v = &mut p.arr.borrow_mut();
    v.remove(p.idx);
}

/// Copy `s[..]` to `p.arr[p.idx..(p.idx + s.end - s.beg)]`.
/// The backing arrays of `p` and `s` may be the same or different.
/// If they are the same, the semantics of an in-place `memmove` apply.
pub fn replace_n<T: Clone>(p: &Ptr<T>, s: &Slice<T>) {
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

impl Default for Term {
    fn default() -> Self { Term::Val(Value::Unit) }
}

/// Insert elements in `s` at write pointer `p`.
/// The backing arrays for `p` and `s` must not be the same.
pub fn insert_n<T: Clone + Default>(p: &Ptr<T>, s: &Slice<T>) {
    if let Slice::Some { arr, beg, end } = s {
        assert!(!Rc::ptr_eq(&p.arr, arr));

        let src = &arr.borrow()[*beg..*end];
        let mut dst = p.arr.borrow_mut();

        let n = src.len();
        let old_len = dst.len();

        // shift right: resize + (in-place) memmove
        dst.resize(old_len + n, Default::default());
        memmove_clone(&mut dst, p.idx, p.idx + n, old_len - p.idx);
        // memcpy
        dst[p.idx..p.idx+n].clone_from_slice(src);
    }
}

/// Remove elements of mutable slice `s`.
pub fn remove_n<T>(s: &Slice<T>) {
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

use phox::vm::*;
use phox::vm::heap::{self, ArrayLike};

fn i64_vec(xs: &[i64]) -> Vec<Term> {
    xs.iter().map(|i| Term::Val(Value::I64(*i))).collect()
}

fn as_val<T: ArrayLike<Elem = Term>>(a: &T) -> Value {
    Value::Array(a.slice(0, a.len()))
}

#[test]
fn test_vm_empty_array() {
    let a = heap::array(vec![]);
    assert_eq!(a.len(), 0);
    assert!(a.capacity() == 0);
    assert_eq!(&a.borrow()[..], &[]);
    assert_eq!(as_val(&a).to_string(), "@[]");
}

#[test]
fn test_vm_array() {
    let xs = i64_vec(&[1, 2, 3]);
    let a = heap::array(xs.clone());
    assert_eq!(a.len(), 3);
    assert!(a.capacity() >= 3);
    assert_eq!(&a.borrow()[..], &xs[..]);
    assert_eq!(as_val(&a).to_string(), "@[1, 2, 3]");
}

#[test]
fn test_vm_slice_basic() {
    let a = heap::array(i64_vec(&[10, 20, 30, 40]));
    let s = a.slice(1, 3); // @[20, 30]

    assert_eq!(s.len(), 2);
    assert_eq!(as_val(&s).to_string(), "@[20, 30]");
}

#[test]
fn test_vm_slice_of_slice() {
    let a = heap::array(i64_vec(&[1, 2, 3, 4, 5]));
    let s1 = a.slice(1, 5);     // @[2,3,4,5]
    let s2 = s1.slice(1, 3);    // @[3,4]

    assert_eq!(as_val(&s2).to_string(), "@[3, 4]");
}

#[test]
fn test_vm_extract() {
    let a = heap::array(i64_vec(&[1, 2, 3, 4]));
    let s = a.slice(1, 3); // @[2,3]

    let b = heap::extract(&s);

    assert_eq!(as_val(&b).to_string(), "@[2, 3]");
    assert!(!std::rc::Rc::ptr_eq(&a, &b)); // 別配列
}

#[test]
fn test_vm_remove_n_middle() {
    let a = heap::array(i64_vec(&[1, 2, 3, 4, 5]));
    let s = a.slice(1, 4); // remove @[2,3,4]

    heap::remove_n(&s);

    assert_eq!(as_val(&a).to_string(), "@[1, 5]");
}

#[test]
fn test_vm_insert_1() {
    let a = heap::array(i64_vec(&[1, 3, 4]));
    let p = a.ptr(1);

    heap::insert_1(&p, Term::Val(Value::I64(2)));

    assert_eq!(as_val(&a).to_string(), "@[1, 2, 3, 4]");
}

#[test]
fn test_vm_insert_n() {
    let a = heap::array(i64_vec(&[1, 4]));
    let b = heap::array(i64_vec(&[2, 3]));

    let p = a.ptr(1);
    let s = b.slice(0, 2);

    heap::insert_n(&p, &s);

    assert_eq!(as_val(&a).to_string(), "@[1, 2, 3, 4]");
}

#[test]
fn test_vm_wtite_n_nonoverlapping() {
    let a = heap::array(i64_vec(&[1, 2, 3, 4]));
    let b = heap::array(i64_vec(&[9, 9]));

    let p = a.ptr(1);
    let s = b.slice(0, 2);

    heap::replace_n(&p, &s);

    assert_eq!(as_val(&a).to_string(), "@[1, 9, 9, 4]");
}

#[test]
fn test_vm_replace_n_overlapping_forward() {
    // src = @[2,3], dst = position 0
    let a = heap::array(i64_vec(&[1, 2, 3, 4]));
    let s = a.slice(1, 3); // @[2,3]
    let p = a.ptr(0);

    heap::replace_n(&p, &s);

    assert_eq!(as_val(&a).to_string(), "@[2, 3, 3, 4]");
}

#[test]
fn test_vm_replace_n_overlapping_backward() {
    // src = @[1,2], dst = position 2
    let a = heap::array(i64_vec(&[1, 2, 3, 4]));
    let s = a.slice(0, 2); // @[1,2]
    let p = a.ptr(2);

    heap::replace_n(&p, &s);

    assert_eq!(as_val(&a).to_string(), "@[1, 2, 1, 2]");
}

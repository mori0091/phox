use phox::api::eval;

#[test]
fn test_user_defined_pair() {
    let (val, sch) = eval(
        "type Pair a b = MkPair a b;
         MkPair 1 true"
    ).unwrap();
    assert_eq!(format!("{}", val), "MkPair 1 true");
    assert_eq!(format!("{}", sch.pretty()), "Pair Int Bool");
}

#[test]
fn test_user_defined_pair_match() {
    let (val, sch) = eval(
        "type Pair a b = MkPair a b;
         match(MkPair 10 20) { MkPair x y => x + y }"
    ).unwrap();
    assert_eq!(format!("{}", val), "30");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_user_defined_result_like() {
    let (val, sch) = eval(
        "type MyResult a e = MyOk a | MyErr e;
         (MyOk 42, MyErr false)"
    ).unwrap();
    assert_eq!(format!("{}", val), "(MyOk 42, MyErr false)");
    assert_eq!(format!("{}", sch.pretty()), "∀ a b. (MyResult Int a, MyResult b Bool)");
}

#[test]
fn test_user_defined_record_variant() {
    let (val, sch) = eval(
        "type Point = P @{ x: Int, y: Int };
         P @{ x: 3, y: 4 }"
    ).unwrap();
    assert_eq!(format!("{}", val), "P @{ x: 3, y: 4 }");
    assert_eq!(format!("{}", sch.pretty()), "Point");
}

#[test]
fn test_user_defined_record_variant_match() {
    let (val, sch) = eval(
        "type Point = P @{ x: Int, y: Int };
         match(P @{ x: 3, y: 4 }) { P @{ x, y } => x + y }"
    ).unwrap();
    assert_eq!(format!("{}", val), "7");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_user_defined_tree_leaf() {
    let (val, sch) = eval(
        "type Tree a = Leaf a | Node (Tree a) (Tree a);
         Leaf 42"
    ).unwrap();
    assert_eq!(format!("{}", val), "Leaf 42");
    assert_eq!(format!("{}", sch.pretty()), "Tree Int");
}

#[test]
fn test_user_defined_tree_node() {
    let (val, sch) = eval(
        "type Tree a = Leaf a | Node (Tree a) (Tree a);
         Node (Leaf 1) (Leaf 2)"
    ).unwrap();
    assert_eq!(format!("{}", val), "Node (Leaf 1) (Leaf 2)");
    assert_eq!(format!("{}", sch.pretty()), "Tree Int");
}

#[test]
fn test_user_defined_tree_match() {
    let (val, sch) = eval(
        "type Tree a = Leaf a | Node (Tree a) (Tree a);
         match(Node (Leaf 10) (Leaf 20)) {
             Leaf x => x,
             Node l r => match(l) { Leaf x => x, Node _ _ => 0 }
         }"
    ).unwrap();
    assert_eq!(format!("{}", val), "10");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_user_defined_tree_record_leaf() {
    let (val, sch) = eval(
        "type Tree a = Leaf @{ value: a } | Node @{ left: Tree a, right: Tree a };
         Leaf @{ value: 42 }"
    ).unwrap();
    assert_eq!(format!("{}", val), "Leaf @{ value: 42 }");
    assert_eq!(format!("{}", sch.pretty()), "Tree Int");
}

#[test]
fn test_user_defined_tree_record_node() {
    let (val, sch) = eval(
        "type Tree a = Leaf @{ value: a } | Node @{ left: Tree a, right: Tree a };
         Node @{ left: Leaf @{ value: 1 }, right: Leaf @{ value: 2 } }"
    ).unwrap();
    assert_eq!(format!("{}", val), "Node @{ left: Leaf @{ value: 1 }, right: Leaf @{ value: 2 } }");
    assert_eq!(format!("{}", sch.pretty()), "Tree Int");
}

#[test]
fn test_user_defined_tree_record_match() {
    let (val, sch) = eval(
        "type Tree a = Leaf @{ value: a } | Node @{ left: Tree a, right: Tree a };
         match(Node @{ left: Leaf @{ value: 10 }, right: Leaf @{ value: 20 } }) {
             Leaf @{ value: x } => x,
             Node @{ left, right } =>
                 match(left) {
                     Leaf @{ value: y } => y,
                     Node _ => 0
                 }
         }"
    ).unwrap();
    assert_eq!(format!("{}", val), "10");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

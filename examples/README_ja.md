# 📚 サンプルコード集

## 1. 恒等関数
```ml
let id = λx. x;
(id 42, id true)
// => (42, true): (Int, Bool)
```

```sh
cargo run examples/identity.phx
```

---

## 2. カリー化と部分適用
```ml
let add = λx. λy. x + y;
let inc = add 1;
(inc 41, add 2 3)
// => (42, 5): (Int, Int)
```

```sh
cargo run examples/curry.phx
```

---

## 3. タプルとレコード
```ml
let t = (1, true, ());
let r = @{ x: 10, y: 20 };
(t.0, r.x)
// => (1, 10): (Int, Int)
```

```sh
cargo run examples/tuple_and_record.phx
```

---

## 4. パターンマッチ
```ml
type Option a = Some a | None;

let getOrZero = λopt.
  match (opt) {
    Some x => x,
    None   => 0
  };

(getOrZero (Some 42), getOrZero None)
// => (42, 0): (Int, Int)
```

```sh
cargo run examples/pattern_match.phx
```

---

## 5. レコードパターン
```ml
let f = λ@{ x, y }. x + y;
f @{ x: 10, y: 32 }
// => 42: Int
```

```sh
cargo run examples/record_pattern.phx
```

---

## 6. newtype略記法
```ml
type Point a = @{ x: a, y: a };

let p = Point @{ x: 3, y: 4 };
p.x + p.y
// => 7: Int
```

```sh
cargo run examples/newtype_record.phx
```

```ml
type Point a = (a, a);

let p = Point (3, 4);
p._0 + p._1
// => 7: Int
```

```sh
cargo run examples/newtype_tuple.phx
```

---

## 7. 複数バリアントの型
```ml
type Result a e = Ok a | Err e;

let unwrapOr = λr. λdefault.
  match (r) {
    Ok x  => x,
    Err _ => default
  };

(unwrapOr (Ok 42) 0, unwrapOr (Err ()) 0)
// => (42, 0): (Int, Int)
```

```sh
cargo run examples/type_with_multiple_variants.phx
```

---

## 8. 高階関数と演算子
```ml
let add = λx. λy. x + y;
let applyTwice = λf. λx. f (f x);
applyTwice (add 1) 40
// => 42: Int
```

```sh
cargo run examples/higher_order_function.phx
```

> Phox ではユーザー定義の演算子も可能です。
> 例えば、次のように冪乗演算子 (**) を定義できます。
> これにより 2 ** 3 のように自然な記法で利用できます。

```ml
let rec (**) = λx. λy.
  if (y <= 0) 1
  else x * (x ** (y - 1));

(2 ** 3, 3 ** 4)
// => (8, 81)
```

```sh
cargo run examples/operator_pow.phx
```

---

## 9. 再帰関数
```ml
let rec fact = λn.
  if (n == 0) 1 else n * fact (n - 1);

fact 5
// => 120: Int
```

```sh
cargo run examples/fact.phx
```

---

## 10. ブロック式
```ml
{
  let x = 10;
  let y = 32;
  x + y
}
// => 42: Int
```

```sh
cargo run examples/block_expr.phx
```

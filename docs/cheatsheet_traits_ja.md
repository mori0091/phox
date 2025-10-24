# トレイト利用例 (チートシート)

このページでは、Phox のトレイト機能の典型的な利用例をまとめています。  
チュートリアルというよりは「事例集」「チートシート」として、必要なときに参照できるように構成しています。

## 基本編
トレイトの定義と実装、そしてそれを使う基本的な方法を示します。  
`Eq` を例に、Int や Bool に対する利用やトレイトレコードの扱い方を確認できます。

### トレイトとインスタンス
```ml
trait Eq a {
  eq  : a -> a -> Bool;
  neq : a -> a -> Bool;
};

impl Eq Int {
  eq  = \a.\b. a == b;
  neq = \a.\b. a != b;
};

eq 1 1
// => true : Bool
```

### トレイトレコード
```ml
let d = @{Eq Int};
d.eq 1 2
// => false : Bool
```

### 中置記法
```ml
let d = @{Eq Int};
1 `d.eq` 1
// => true : Bool
```

### ファーストクラスな利用
```ml
let f = \@{eq, neq}.\x.\y. eq x y;
f @{Eq Int} 3 4
// => false : Bool
```

### 多相性
```ml
impl Eq Bool {
  eq  = \a.\b. a == b;
  neq = \a.\b. a != b;
};

eq true false
// => false : Bool
```

---

## エラー編
未実装のトレイトやトレイトレコードを使おうとした場合のエラー例です。  
暗黙のフォールバックはなく、必ず明示的な実装が必要になります。

### 未実装のトレイトを使うとエラー
```ml
trait Eq a { eq : a -> a -> Bool; };
eq true false
// infer error: UnboundVariable
```

### 未実装のトレイトレコードを作ろうとするとエラー
```ml
trait Eq a { eq : a -> a -> Bool; };
@{Eq Bool}
// resolve error: no implementation for Eq Bool
```

---

## 曖昧さと解決
同じ名前のメンバを持つ複数のトレイトを実装した場合など、曖昧さが発生するケースです。  
その場合はトレイトレコードを明示的に指定することで解決できます。

### メンバ名の衝突による曖昧さ
```ml
trait Foo a { f : a -> a; };
trait Bar a { f : a -> a; };

impl Foo Int { f = \x. x; };
impl Bar Int { f = \x. x; };

f 100
// infer error: ambiguous variable `f`
// candidates: Bar Int => Int -> Int, Foo Int => Int -> Int
// hint: use @{Bar Int}.f or @{Foo Int}.f
```

### 明示的に disambiguation
```ml
@{Bar Int}.f 100
// => 100 : Int

@{Foo Int}.f 100
// => 100 : Int
```

### 型が曖昧な場合のエラー
```ml
trait Foo a { f : a -> a; };
impl Foo Bool { f = \x. x; };
impl Foo Int  { f = \x. x; };

f
// infer error: ambiguous variable `f`
// candidates: Foo Bool => Bool -> Bool, Foo Int => Int -> Int
// hint: use @{Foo Bool}.f or @{Foo Int}.f
```

# Phox

![CI](https://github.com/mori0091/phox/actions/workflows/ci.yaml/badge.svg)
![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)
![Rust](https://img.shields.io/badge/rust-1.80+-orange)

Phox is a small functional programming language with:
- **Hindley–Milner type inference**,
- **algebraic data types**,
- **pattern matching**,
- **multi-parameter typeclasses (`trait`/`impl`)**,
- **requires (trait-bounds) inference**,
- **generic function template (`*let`)**,
- **automatic type/trait resolution without type-annotation**,
- **iterator / generator pipeline**,
- Rust-like **module system**,
- and more.

It aims to be a simple yet expressive tool — your clever companion for exploring type theory and practical programming.

> Note: Phox currently has no string type or I/O.  
> That means you can’t even write “Hello, World!” yet —  
> but you can prove that `fact 5 = 120` with full type safety 😉

See also <https://mori0091.github.io/phox-book/>

## 📑 Table of Contents

- ✨ [Features](#-features)
- 📘 [Language Overview](#-language-overview)
- 💡 [Sample Programs](#-sample-programs)
- 🚀 [Getting Started](#-getting-started)
- 🧭 [Roadmap](#-roadmap)
- 📄 [License](#-license)

_New to Phox? Start with [🚀 Getting Started](#-getting-started)._

---

## ✨ Features

- **Hindley–Milner type inference**  
  No need to annotate types in most cases. (In fact, Phox has no type-annotation syntax)
- **Algebraic data types (ADT)**  
  Define expressive data structures with variants.
- **Pattern matching**  
  Concise and powerful destructuring.
- **Newtype shorthand**  
  Cleaner syntax for single-constructor wrapper types.
- **First-class functions**  
  Functions are values, operators are functions too.
- **Generic function temlates**  
  Non-first class / overloadable generic function templates.
- **Multi-parameter typeclasses (`trait`/`impl`)**  
  Define the relationship between multiple methods and multiple types.
- **Trait record**  
  Typeclasses (`trait`/`impl`) can be instansiated as a first-class record value, with a simple syntax.
- **Module system**  
  Rust like module / namespace definition.
- **Simple syntax**  
  Inspired by ML-family languages, with a focus on clarity.

---

## 📘 Language Overview

> - In code examples, `// => ` shows the result as a comment.
> - In REPL examples, `=>` shows the evaluated result.

### Semicolons

- `;` separates multiple items (type declarations / statements / expressions) in a block or at the top level.
- Each item is evaluated in order; only the last expression's value is returned.
- If a block or top-level input ends with `;`, an implicit `()` is added.

``` rust
// Multiple items in a block
{
    let x = 1; // => (): ()  (discarded)
    let y = 2; // => (): ()  (discarded)
    x + y;     // => 3: Int  (discarded)
    2 * x + y  // => 4: Int  (result)
}
// => 4: Int
```

``` rust
// Multiple items in the top level
let x = 1; // => (): ()  (discarded)
let y = 2; // => (): ()  (discarded)
x + y;     // => 3: Int  (discarded)
2 * x + y  // => 4: Int  (result)
// => 4: Int
```

``` rust
// Items in a block ends with `;`
{
    1 + 2; // => 3: Int  (discarded)
}
// => (): ()
```

``` rust
// Items in the top level ends with `;`
1 + 2; // => 3: Int  (discarded)
// => (): ()
```

``` rust
// No items in a block
{}
// => (): ()
```

### Type definitions
``` rust
type Option a = Some a | None;
type Pair a b = Pair a b;
type Result e a = Ok a | Err e;
```

- Variants can take **0 or more arguments**.
- **Newtype shorthand** is available when:
  - There is only one variant, and
  - The type name and constructor name are the same, and
  - The variant has exactly one tuple, one record, or one array argument.

``` rust
// Normal form
type Point a = Point @{ x: a, y: a };

// Newtype shorthand
type Point a = @{ x: a, y: a };
type Wrapper a = (a,);
```

### Pattern matching
``` rust
match (opt) {
  Some x => x,
  None   => 0
}
```

### Tuples, records, and arrays
``` rust
let t = (1, true, ());
let r = @{ x = 10, y = 20 };
let a = @[1, 2, 3];
(t.1, r.x, a[0])   // tuple index and array index is 0-based
// => (true, 10, 1): (Bool, Int, Int)
```

👉 Field access is also available for **newtype shorthand** types.

``` rust
type Point a = @{ x: a, y: a };

let p = Point @{ x = 3, y = 4 };
p.x + p.y
// => 7: Int
```

``` rust
type Point a = (a, a);

let p = Point (3, 4);
p.0 + p.1
// => 7: Int
```

``` rust
type Point a = @[a];

let p = Point @[3, 4];
p[0] + p[1]
// => 7: Int
```

### Functions as operators
Function `f(x) = x + x` can be defined like this:

``` rust
let f = λx. 0 + x + x;
// let f = \x. 0 + x + x;    // same as the above.
f 3
// => 6: Int
```

``` rust
let normSq = λx.λy. 0 + x * x + y * y;
(normSq 2 3, 3 `normSq` 4)
// => (13, 25): (Int, Int)
```

- Infix notation with backticks allows any function to be used as an operator.

> [!NOTE]
> Why we use `0 + ...` ?  
> Because primitive operators such as `+` are overloaded monomorphic functions.  
> 👉 For more details (with Japanese explanations), see :
> - [Trait Resolution and the Difference Between `let` and `*let`](docs/trait_resolution_en.md).
> - [トレイト解決と `let` / `*let` の違い](docs/trait_resolution_ja.md).

### Operators as functions / User-defined operators
``` rust
// User-defined operator
let rec (**) = λx.λy.
    if (0 >= y) 1
    else x * x ** (y - 1);

((**) 2 3, 3 ** 4)
// => (8, 81): (Int, Int)
```

- User-defined operators and also Built-in operators like `==`, can always be used both infix and prefix.

``` sh
(==) 1 1  // => true: Bool
1 == 1    // => true: Bool
```

``` sh
(-) 3 1   // => 2: Int   (binary minus as a function)
negate 3  // => -3: Int  (unary minus as a function)
```

``` sh
let x = 3;
-x        // => -3: Int  (unary minus; syntax sugar of `negate x`)
```

``` sh
! true    // => false: Bool  (unary not)
(!) true  // => false: Bool  (unary not; prefix form of `!`)
not true  // => false: Bool  (unary not)
```

### Pipeline and Composition operators

Phox provides **pipeline operators** and **composition operators** to make function application and chaining more readable.  
They allow you to express nested calls as a clear flow of data or a sequence of transformations.

#### Pipeline operators
- `x |> f` is the same as `f x`.  
- `f <| x` is also the same as `f x`.  
- Use `|>` when you want to read left‑to‑right, and `<|` when you prefer right‑to‑left.

``` rust
x |> f |> g    // g (f x)
g <| f <| x    // g (f x)
```

#### Composition operators
- `f >> g` creates a function that applies `f` first, then `g`.  
- `f << g` creates a function that applies `g` first, then `f`.

``` rust
(f >> g) x    // g (f x)
(f << g) x    // f (g x)
```

#### Precedence
- Composition operators `>>` / `<<` bind more tightly than pipeline operators.  
- Among pipelines, `|>` binds more tightly than `<|`.

``` rust
x |> f >> g    // x |> (f >> g)
f <| x |> g    // f <| (x |> g)
```

``` rust
let f = \x. x + 1;
let g = \x. x * 2;
let h = \x. x * x;
1 |> f |> g |> h;       // => 16: Int  ; h (g (f 1))
1 |> f >> g >> h;       // => 16: Int  ; h (g (f 1))
f >> g >> h <| 1;       // => 16: Int  ; h (g (f 1))
f >> g >> h <| 1 |> f;  // => 36: Int  ; h (g (f (f 1)))
```

👉 In short: **use pipelines to pass data, and composition to connect functions**. You can freely combine both styles to write in the way that feels most natural.

### Infix-operator partial applications (a.k.a. section syntax)
Assuming that `op` is a arbitrary infix-operator:
- `|(e op)` or `|(e op _)` are same as `\rhs. e op rhs` (bind the 1st argument of `op` with `e`)
- `|(op e)` or `|(_ op e)` are same as `\lhs. lhs op e` (bind the 2nd argument of `op` with `e`)

``` rust
let f = |(1 +);
f 2    // 1 + 2
// => 3
```

``` rust
let f = |(/ 2);
f 6    // 6 / 2
// => 3
```

### Pure type-safe `for`/`while` loop functions

``` rust
let fact = \n. {
  let init      = (1, n);
  let predicate = \(_, n'). 0 < n';
  let update    = \(a, n'). (a * n', n' - 1);
  let (res, _) = for init predicate update;
  res
};

fact 5
// => (120, 0): (Int, Int)
```

`while pred upd` is same as `\init. for init pred upd`.

### Traits and Implementations
Phox supports **traits** (`trait`; similar to type classes) to define shared behavior across types.

``` rust
trait Eq2 a {
  eq  : a -> a -> Bool;
  neq : a -> a -> Bool;
}
```

An **implementation** (`impl`) provides concrete definitions for a trait and a type:

``` rust
impl Eq2 Int {
  eq  = λx.λy. @{Eq Int}.(==) x y;
  neq = λx.λy. @{Eq Int}.(!=) x y;
};

impl Eq2 Bool {
  eq  = λx.λy. @{Eq Bool}.(==) x y;
  neq = λx.λy. @{Eq Bool}.(!=) x y;
};
```

Now you can use `eq` and `neq` with `Int` or `Bool` values:

``` rust
eq 2 2        // => true
eq 2 3        // => false
neq 2 3       // => true

eq true true  // => true
eq true false // => false
neq true true // => false

2 `eq` 2      // => true (infix notation)
2 `eq` 3      // => false
```

### Trait Records
A **trait record** is a first-class value representing a trait implementation.  
You can explicitly pass or select an implementation:

``` rust
let eqInt = @{Eq Int};   // trait record for `Eq Int`

eqInt.(==) 2 3     // => false
2 `eqInt.(==)` 3   // => false
```

Trait records make resolution explicit and predictable.  
If multiple candidates exist, ambiguity is reported as an error, and you can disambiguate by using a trait record.


👉 For more examples (with Japanese explanations), see [トレイト利用例 (チートシート)](docs/cheatsheet_traits_ja.md).

### Iterators, generators, and sink

Arrays itself is also an iterator.  
`fold` consumes all inputs and performs folding function.
``` rust
@[1,2,3,4,5] |> fold (*) 1
// => 120: Int
```

`counter` generates infinite sequence of integers.
``` rust
counter 1 |> take 5 |> fold (*) 1
// => 120: Int
```

`filter` filters elements by predicate function.  
`collect` consumes all inputs and built result.
``` rust
counter 1 |> filter (|(% 2) >> |(== 0)) |> take 5 |> collect Nil
// => Cons 2 (Cons 4 (Cons 6 (Cons 8 (Cons 10 Nil)))): List Int
```
``` rust
counter 1 |> filter (|(% 2) >> |(== 0)) |> take 5 |> collect @[]
// => @[2, 4, 6, 8, 10]: @[Int]
```

The `::core::iter` module provides much more iterators / iterator-adapters, such as `zip`, `zip_with`, `enumerate`, etc.

👉 For more details, see also [`::core::iter` module](assets/core/iter.phx).


### Arrays API

Phox arrays (`@[a]`) are **pure functional**, **reference-transparent**, and support **O(1) slicing** with **copy‑on‑write append**.  
They behave like immutable vectors with a clean, orthogonal API.

``` rust
push @[1,2,3] 4
// => @[1, 2, 3, 4]: @[Int]

pop @[1,2,3]
// => Some (@[1, 2], 3): Option (@[Int], Int)

uncons @[1,2,3]
// => Some (1, @[2, 3]): Option (Int, @[Int])

collect @[1,2,3] @[4,5,6]
// => @[1, 2, 3, 4, 5, 6]: @[Int]

collect @[1,2,3] <| Cons 10 <| Cons 20 <| Cons 30 Nil
// => @[1, 2, 3, 10, 20, 30]: @[Int]

@[10, 20, 30] |> collect @[1,2,3]
// => @[1, 2, 3, 10, 20, 30]: @[Int]

(Cons 10 <| Cons 20 <| Cons 30 Nil) |> collect @[1,2,3]
// => @[1, 2, 3, 10, 20, 30]: @[Int]

insert1 @[1,2,3] 1 10
// => @[1, 10, 2, 3]: @[Int]

insertN @[1,2,3] 1 @[10,20]
// => @[1, 10, 20, 2, 3]: @[Int]

remove1 @[1,2,3,4] 1
// => @[1, 3, 4]: @[Int]

removeN @[1,2,3,4] 1 3
// => @[1, 4]: @[Int]

replace1 @[1,2,3,4] 1 10
// => @[1, 10, 3, 4]: @[Int]

replaceN @[1,2,3,4] 1 3 @[10,20,30]
// => @[1, 10, 20, 30, 4]: @[Int]
```

👉 For more details, see also [`::core::array` module](assets/core/array.phx).

---

## 💡 Sample Programs

### Identity
``` rust
let id = λx. x;
id 42
// => 42: Int
```

### Factorial
``` rust
// recursive function version.
let rec fact = λn.
  if (0 == n) 1 else n * fact (n - 1);

fact 5
// => 120: Int
```

``` rust
// iterator pipeline version.
let fact = λn.
  counter 1 |> take n |> fold (*) 1;

fact 5
// => 120: Int
```

### Option
``` rust
type Option a = Some a | None;

let getOrZero = λopt.
  match (opt) {
    Some x => x,
    None   => 0
  };

getOrZero (Some 42)
// => 42: Int
```

### Result
``` rust
type Result e a = Ok a | Err e;

let unwrapOr = λr. λdefault.
  match (r) {
    Ok x  => x,
    Err _ => default
  };

unwrapOr (Err ()) 0
// => 0: Int
```


👉 More examples (with Japanese explanations) are available in [examples/README_ja.md](examples/README_ja.md).

---

## 🚀 Getting Started

> ⚠️ Work in progress — Phox is under active development.

### Build

Clone this repository and build with Rust (tested on 1.80+, may work on earlier versions):

```sh
cargo build
```

### Run in REPL

If you run without arguments, Phox starts an interactive REPL:

``` sh
cargo run
> let rec fact = λn. if (0 == n) 1 else n * fact (n - 1);
> fact 5
=> 120: Int
```

In REPL, an input starts with `:` is recognized as a REPL command.  
For example, `:?` shows the list of available commands:

``` sh
cargo run
> :?

:quit, :q
    exit REPL.

:help, :h, or :?
    print this help messages.

:load <path>, :l <path>
    load and evaluate Phox source file specified by <path>.

```

### Run a program file

Pass a `.phx` file to execute it:

> - `.phx` is the conventional extension for Phox source files (plain text).  
> - `.txt` files are also accepted.

``` sh
cargo run examples/fact.phx
=> 120: Int
```

### Run from stdin

You can also pipe code from stdin (use `-` explicitly):

```sh
echo "1 + 2" | cargo run -
=> 3: Int
```

Resulting `value` and inferred `type` are printed on success, like this:

``` sh
=> value: type
```

On error, an error message is printed, like this:

``` sh
parse error: UnrecognizedToken { /* snip... */ }
```

Example programs are available in the `examples/` directory.

---

## 🧭 Roadmap

### REPL
- [X] REPL with type inference output

### Core language
- [X] Standard library (see `assets/` directory)
- [X] Trait system (multi-parameter type classes)
- [X] Module system
- [X] Function template `*let`
- [ ] ~~Row polymorphism~~
- [ ] ~~Nat kind (type-level natural numbers)~~

### Proc system
- [ ] Procedures with side-effects
- [ ] Dynamic arrays (mutable / growable arrays)

### Optimization
- [ ] Unboxed arrays
- [ ] Alias analysis and reuse analysis of heap memory.

### Trait system
- [X] Constraint-based multi-parameter type classes (`trait`/`impl`)
- [X] Automatic requrirements inference for `impl`s
- [X] Higher order trait-record inference (e.g. `@{Iter s a}`)

### Record system
- [X] Trait record
- [ ] ~~Split/Merge operators~~
- [ ] ~~Row polymorphism~~

---

## 📄 License

MIT License

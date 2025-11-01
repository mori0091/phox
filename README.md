# Phox

![CI](https://github.com/mori0091/phox/actions/workflows/ci.yaml/badge.svg)
![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)
![Rust](https://img.shields.io/badge/rust-1.80+-orange)

Phox is a small functional programming language with **Hindley–Milner type inference**, **algebraic data types**, and **pattern matching**.  
It aims to be a simple yet expressive tool — your clever companion for exploring type theory and practical programming.

> Note: Phox currently has no string type or I/O.  
> That means you can’t even write “Hello, World!” yet —  
> but you can prove that `fact 5 = 120` with full type safety 😉

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
  No need to annotate types in most cases.
- **Algebraic data types (ADT)**  
  Define expressive data structures with variants.
- **Pattern matching**  
  Concise and powerful destructuring.
- **Newtype shorthand**  
  Cleaner syntax for single-constructor wrapper types.
- **First-class functions**  
  Functions are values, operators are functions too.
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

``` ml
// Multiple items in a block
{
    let x = 1; // => (): ()  (discarded)
    let y = 2; // => (): ()  (discarded)
    x + y;     // => 3: Int  (discarded)
    2 * x + y  // => 4: Int  (result)
}
// => 4: Int
```

``` ml
// Multiple items in the top level
let x = 1; // => (): ()  (discarded)
let y = 2; // => (): ()  (discarded)
x + y;     // => 3: Int  (discarded)
2 * x + y  // => 4: Int  (result)
// => 4: Int
```

``` ml
// Items in a block ends with `;`
{
    1 + 2; // => 3: Int  (discarded)
}
// => (): ()
```

``` ml
// Items in the top level ends with `;`
1 + 2; // => 3: Int  (discarded)
// => (): ()
```

``` ml
// No items in a block
{}
// => (): ()
```

### Type definitions
```ml
type Option a = Some a | None;
type Pair a b = Pair a b;
type Result a e = Ok a | Err e;
```

- Variants can take **0 or more arguments**.
- **Newtype shorthand** is available when:
  - There is only one variant, and
  - The type name and constructor name are the same, and
  - The variant has exactly one tuple or one record argument.

```ml
// Normal form
type Point a = Point @{ x: a, y: a };

// Newtype shorthand
type Point a = @{ x: a, y: a };
type Wrapper a = (a,);
```

### Pattern matching
```ml
match (opt) {
  Some x => x,
  None   => 0
}
```

### Tuples and records
```ml
let t = (1, true, ());
let r = @{ x: 10, y: 20 };
(t.1, r.x)   // tuple index is 0-based
// => (true, 10): (Bool, Int)
```

👉 Field access is also available for **newtype shorthand** types.

```ml
type Point a = @{ x: a, y: a };

let p = Point @{ x: 3, y: 4 };
p.x + p.y
// => 7: Int
```

```ml
type Point a = (a, a);

let p = Point (3, 4);
p.0 + p.1
// => 7: Int
```

### Functions as operators
Function `f(x) = x + x` can be defined like this:

``` ml
let f = λx. x + x;
// let f = \x. x + x;    // same as the above.
f 3
// => 6: Int
```

``` ml
let normSq = λx.λy. x * x + y * y;
(normSq 2 3, 3 `normSq` 4)
// => (13, 25): (Int, Int)
```

- Infix notation with backticks allows any function to be used as an operator.

### Operators as functions / User-defined operators
```ml
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

### Traits and Implementations
Phox supports **traits** (`trait`; similar to type classes) to define shared behavior across types.

```ml
trait Eq2 a {
  eq  : a -> a -> Bool;
  neq : a -> a -> Bool;
}
```

An **implementation** (`impl`) provides concrete definitions for a trait and a type:

```ml
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

```ml
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

```ml
let eqInt = @{Eq Int};   // trait record for `Eq Int`

eqInt.(==) 2 3     // => false
2 `eqInt.(==)` 3   // => false
```

Trait records make resolution explicit and predictable.  
If multiple candidates exist, ambiguity is reported as an error, and you can disambiguate by using a trait record.


👉 For more examples (with Japanese explanations), see [トレイト利用例 (チートシート)](docs/cheatsheet_traits_ja.md).

---

## 💡 Sample Programs

### Identity
```ml
let id = λx. x;
id 42
// => 42: Int
```

### Factorial
```ml
let rec fact = λn.
  if (0 == n) 1 else n * fact (n - 1);

fact 5
// => 120: Int
```

### Option
```ml
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
```ml
type Result a e = Ok a | Err e;

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

- [X] REPL with type inference output
- [ ] Standard library (currently `Option`/`List` are built-in types as a temporary measure; will be moved out as a library)
- [ ] Module system
- [X] Constraint-based type classes (future; Haskell-style type classes)

---

## 📄 License

MIT License

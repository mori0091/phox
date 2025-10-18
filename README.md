# Phox

![CI](https://github.com/mori0091/phox/actions/workflows/ci.yaml/badge.svg)
![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)
![Rust](https://img.shields.io/badge/rust-1.80+-orange)

Phox is a small functional programming language with **Hindley–Milner type inference**, **algebraic data types**, and **pattern matching**.  
It aims to be a simple yet expressive tool — your clever companion for exploring type theory and practical programming.

> Note: Phox currently has no string type or I/O.  
> That means you can’t even write “Hello, World!” yet —  
> but you can prove that `fact 5 = 120` with full type safety 😉

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

### Functions as operators
``` ml
let normSq = λx.λy. x * x + y * y;
(normSq 2 3, 3 `normSq` 4)
// => (13, 25): (Int, Int)
```

- Infix notation with backticks allows any function to be used as an operator.

### Operators as functions / User-defined operators
```ml
let rec (**) = λx.λy.
    if (y <= 0) 1
    else x * x ** (y - 1);

((**) 2 3, 3 ** 4)
// => (8, 81): (Int, Int)
```

- Built-in operators like == are currently infix-only, but user-defined operators can always be used as functions.

---

## 📚 Sample Programs

### Identity
```ml
let id = λx. x;
id 42
// => 42: Int
```

### Factorial
```ml
let rec fact = λn.
  if (n == 0) 1 else n * fact (n - 1);

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

1. Clone this repository
2. Build with Rust (tested on 1.80+, may work on earlier versions)
3. Run with `.phx` files

```sh
cargo build
cargo run examples/hello.phx
```

You can also pipe code from stdin:

```sh
echo "1 + 2" | cargo run
```

Example programs are available in the `examples/` directory.

---

## 🛠 Roadmap

- [ ] REPL with type inference output
- [ ] Standard library (Option, Result, List, etc.)
- [ ] Module system
- [ ] Constraint-based type classes (future)

---

## 📄 License

MIT License

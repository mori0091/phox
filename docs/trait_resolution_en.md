# Trait Resolution and the Difference Between `let` and `*let`

Primitive operators in Phox, such as `+`, are provided by traits (e.g., `Add a`).  
When multiple implementations of a trait exist for different types,  
**which implementation is used is determined by scope and binding**.

## `let` is monomorphic
A function defined with `let` must have **a single, fully determined type at the point of definition**.  
Therefore, the following definition is ambiguous and results in an error:

```rust
let add = \a.\b. a + b;
// => ambiguous variable `(+)`
```

At the point of `a + b`, the type of `a` is not yet known,  
so the compiler cannot decide whether to use `Add Int` or `Add u8`.

> [!NOTE]  
> Depending on context, `let` *can* become polymorphic.  
> For example, at the module top level, the following definition becomes a polymorphic function:
> ```rust
> // id : ∀ a. a -> a
> let id = \x. x;
> ```
>
> In contrast, the earlier `add` example contains an overloaded operator `(+)`  
> with multiple candidate implementations.  
> Because the type cannot be uniquely determined, the definition is monomorphic and ambiguous.

> [!NOTE]  
> A `let` binding becomes polymorphic **only when its definition contains no ambiguity**.  
> For example, `let id = \x. x` has no constraints on `x`, so it generalizes.  
>
> However, an expression like `a + b` requires trait resolution.  
> If the compiler cannot determine which trait implementation to use at the definition site,  
> the `let` binding cannot be generalized.  
>
> In short:
> **A `let` binding is polymorphic only when its type can be uniquely inferred at definition time.**

## `*let` is polymorphic
`*let` defines a **constrained polymorphic function**.  
Trait implementations are **statically resolved at compile time, based on the context in which the function is used**.  
(This is not dictionary passing; the appropriate implementation is *statically baked into* each use site.)

```rust
*let add = @{Add a}.(+);

add 2 3       // Add Int is baked in
add 2u8 3u8   // Add u8 is baked in
```

> `*let` allows ambiguous definitions,  
> but if the implementation is still ambiguous *at the use site*, it becomes an error.

## Type determination by literals
A literal can force a specific type, resolving ambiguity:

```rust
let add = \a.\b. 0 + a + b;
// 0 is Int, so Add Int is selected
```

## Fixing an operator implementation in a local scope
Inside a block `{ ... }`, you can bind a trait record to restrict which implementation is used:

```rust
let add = {
  let @{(+)} = @{Add Int};
  \a.\b. a + b
};

add 2 3   // => 5: Int
```

This is equivalent to:

```rust
let add = {
  let (+) = @{Add Int}.(+);
  \a.\b. a + b
};
```

Or even more concisely:

```rust
let add = @{Add Int}.(+);

add 2 3   // => 5: Int
```

> This works because the scope contains **exactly one candidate for `(+)`**,  
> eliminating ambiguity.

## Summary
- `let` is monomorphic: the implementation must be uniquely determined at definition time  
- `*let` is polymorphic: the implementation is statically resolved based on usage  
- Literals can force a type and resolve ambiguity  
- `{ let @{(+)} = @{Add Int}; ... }` fixes an implementation within a scope  
- Trait records are **namespace bindings**, not dictionaries  
- When ambiguity remains, Phox always reports an error (there is no implicit default)

> A trait record is not a “dictionary”; it is a **record value**.  
> Combined with pattern matching (e.g., `let @{(+)} = @{Add Int};`),  
> it acts as a **namespace switch** that selects which implementation is used in the current scope.


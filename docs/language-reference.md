# Language Reference

> [!NOTE]
> This documentation is a work in progress.

## Syntax

### Comments

Line comments start with `#`.
To keep the grammar simple, there are no block comments.

TODO

## Functions

TODO

## Statements and expressions

### Control flow

TODO

## Type system

TODO

### Primitive types

There are three primitive types:

- `Num`: 64-bit floating point numbers
- `String`: strings of characters
- `Bool`: `true` or `false`

### Variables

Variables are defined with the `let` keyword:

```sc2
let name = "Buffy Summers"
let vampires-slayed = 500
```

Variables are lexically scoped and can be defined globally, within a sprite or
within a function.

```sc2
let π = 3.14

sprite Player {
    let score = 0

    fn display-score {
        let score-text = score.to-string()
        say(score-text)
    }
}
```

Shadowing is supported, and should be preferred over mutation when possible:

```sc2
let five = 5

# New variable with the same name.
# Note that this has a different type (String instead of Num).
let five = five.to-string()
```

To refer to a variable itself instead of its value, use the `&` syntax.
This can be used to mutate it:

```sc2
let current-episode = 10

fn binge {
    repeat 5 {
        &current-episode = current-episode + 1
    }
}
```

A variable containing a `T` has type `Var[T]`. This type cannot be passed around
at runtime, but you can use it as a `comptime` parameter:

```sc2
fn multiply-by-10(_ comptime var: Var[Num]) {
    # Since `var` is a function parameter, it does not implicitly decay to its
    # value. We must therefore use `get` explicitly.
    #
    # The signature of get is `fn get[T](_ comptime var: Var[T]) -> T`.
    # This means that it can handle variables of any type.
    var = get(var) * 10
}

let n = 5
# Pass a reference to `n`.
multiply-by-10(&n)
```

### Lists

TODO

### Type ascription

In some cases, the type of an expression is ambiguous.
To fix this, you can use type ascription to specify an explicit type:

```sc2
# error: cannot infer type of empty list literal
let my-empty-list-of-numbers = []

# OK
let my-empty-list-of-numbers = [] as List[Num]
```

## Metaprogramming

### Type aliases

> [!WARNING]
> Type aliases are not fully supported yet due to limitations of the compiler's
> intermediate representations. Expect crashes.

Since types are values, variables can be used as type aliases:

```sc2
let Acceleration = Num

fn g -> Acceleration {
    9.82
}
```

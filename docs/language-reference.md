# Language Reference

> [!WARNING]
> This documentation is a work in progress.

## Syntax

SC2 uses a Rust-like syntax with some simplifications
that make semicolons unnecessary.

### Comments

Line comments start with `#`.
To keep the grammar simple, there are no block comments.

### Identifiers

Identifiers use the regex `[\p{XID_Start}_][\p{XID_Continue}-]*`.
Notably, this includes hyphens, which should be used instead of underscores.

Following are some examples of valid identifiers:

- `temp`
- `Type`
- `LOUD-VARIABLE`
- `the-number-42`
- `XML-HTTP-Request`
- `smörgåsbord`
- `π`
- `Δx`

## Functions

TODO

## Statements and expressions

### Control flow

```sc2
if 𝐜𝐨𝐧𝐝𝐢𝐭𝐢𝐨𝐧 {
    # …
}

if 𝐜𝐨𝐧𝐝𝐢𝐭𝐢𝐨𝐧 {
    # …
} else {
    # …
}

if 𝐜𝐨𝐧𝐝𝐢𝐭𝐢𝐨𝐧 {
    # …
} else if 𝐜𝐨𝐧𝐝𝐢𝐭𝐢𝐨𝐧 {
    # …
}

while 𝐜𝐨𝐧𝐝𝐢𝐭𝐢𝐨𝐧 {
    # …
}

until 𝐜𝐨𝐧𝐝𝐢𝐭𝐢𝐨𝐧 {
    # …
}

forever {
    # …
}

repeat 𝐭𝐢𝐦𝐞𝐬 {
    # …
}

# Range 1–𝐭𝐢𝐦𝐞𝐬 inclusive
for 𝐯𝐚𝐫 𝐭𝐢𝐦𝐞𝐬 {
    # …
}

return 𝐯𝐚𝐥𝐮𝐞
```

### Operator overloading

The following binary operators desugar to function calls
and can therefore be overloaded.

| Operator | Function |
|----------|----------|
| +        | add      |
| -        | sub      |
| *        | mul      |
| /        | div      |
| %        | mod      |
| <        | lt       |
| ==       | eq       |
| >        | gt       |
| =        | set      |

## Type system

TODO

### Primitive types

There are three primitive types:

- `Num`: 64-bit floating point numbers
- `String`: strings of characters
- `Bool`: `true` or `false`

### Aggregate types

> [!WARNING]
> Aggregate types are not fully implemented and are not usable yet.

Aggregate types, also known as *product types* or *structs*, consist of named
fields of other types. They are defined with the `struct` keyword:

```sc2
struct Cat {
    name: String,
    age: Number,
    is-fluffy: Bool,
}
```

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
inline fn multiply-by-10(_ comptime var: Var[Num]) {
    # Since `var` is a function parameter, it does not implicitly decay to its
    # value. We must therefore use `get` explicitly.
    #
    # The signature of `get` is `fn get[T](_ comptime var: Var[T]) -> T`.
    # This means that it can handle variables of any type.
    var = get(var) * 10
}

let n = 5
# Pass a reference to `n`.
multiply-by-10(&n)
```

### Lists

A list can contain any number of elements of a single type.
You can create a list by assigning a list literal to a variable:

```sc2
let primes = [2, 3, 5, 7, 11]
```

Lists, unlike primitive types, use reference semantics. This means that each
list literal has a sense of "identity" and reassigning it to a new variable
won't create a new list:

```sc2
let original = [1]
let aliased = original
aliased.push(2)
# `original` now contains 1 and 2.
```

A list of `T`s has type `List[T]` and, like variables, can be used as a
`comptime` function parameter:

```sc2
fn first-number(of comptime numbers: List[Num]) -> Num {
    numbers.at(1)
}

let squares = [1, 4, 9, 16]
let one = first-number(of: squares)
```

There are several intrinsics for list manipulation, see `builtins.sc2`.

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

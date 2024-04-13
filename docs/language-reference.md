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

TODO

### Variables

TODO

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

TODO

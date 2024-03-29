# Architecture

- The lexer turns the source code into a stream of tokens.
- The parser arranges the tokens into a concrete syntax tree (CST).
  - Some syntax errors are detected during parsing while others are checked for
    in the CST afterwards.
  - The CST is used for name resolution since it makes it easy to iterate
    through parent scopes.
- The abstract syntax tree (AST) provides a typed view of the CST.
- The AST gets lowered to a high-level intermediate representation (HIR).
- The HIR gets type-checked, which involves resolving function calls since the
  language supports function overloading.
- Semantic analysis is performed on the HIR to detect certain errors.
- The linter traverses the HIR and emits warnings.
- An early dead code elimination step is performed, which removes (and warns
  about) unused functions to avoid wasting time on them.
- If no errors have occured, the compiler moves on to code generation:
  - The HIR gets lowered to MIR (the mid-level intermediate representation).
  - Inlining is performed on the MIR.
  - Several optimizations are repeatedly applied to the MIR until a fixed point
    is reached.
  - Finally, the MIR gets compiled to a Scratch project.

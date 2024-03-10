(comment) @comment.line

(string_literal) @string
(number_literal) @constant.numeric

[
  "false"
  "true"
] @constant.builtin.boolean

[
  "costumes"
  "inline"
  "comptime"
] @keyword
[
  "if"
  "else"
] @keyword.control.conditional
[
  "while"
  "until"
  "repeat"
  "forever"
  "for"
] @keyword.control.repeat
"fn" @keyword.function
[
  "sprite"
  "let"
] @keyword.storage.type
"as" @keyword.operator

[
  "="
  "->"
  "&"
  "+"
  "-"
  "*"
  "/"
  "%"
  "<"
  "=="
  ">"
] @operator
[
  ":"
  ","
] @punctuation.delimiter
[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
] @punctuation.bracket

(function_definition
  "fn" . (identifier) @function)
(function_call
  (identifier) @function)

(named_argument
  . (identifier) @label)

(generics
  (identifier) @type.parameter)

(
  (identifier) @type.builtin
  (#match? @type.builtin "^(Num|String|Bool|Var|List|Type)$")
)

(identifier) @variable

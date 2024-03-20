module.exports = grammar({
  name: "sc2",

  extras: $ => [/[ \t\n]+/, $.comment],

  word: $ => $.identifier,

  rules: {
    source_file: $ =>
      repeat(
        choice(
          $.import,
          $.sprite,
          $.function_definition,
          $.variable_definition,
        ),
      ),

    import: $ => seq("import", $.string_literal),

    sprite: $ => seq("sprite", $.identifier, $.sprite_body),

    sprite_body: $ =>
      seq(
        "{",
        repeat(
          choice($.function_definition, $.variable_definition, $.costumes),
        ),
        "}",
      ),

    costumes: $ => seq("costumes", "{", repeat($.costume), "}"),

    costume: $ => seq($.string_literal, ":", $.string_literal, optional(",")),

    function_definition: $ =>
      seq(
        optional("inline"),
        "fn",
        $.identifier,
        optional($.generics),
        $.function_parameters,
        optional(seq("->", $._expression)),
        $.block,
      ),

    generics: $ => seq("[", repeat(seq($.identifier, optional(","))), "]"),

    function_parameters: $ => seq("(", repeat($.parameter), ")"),

    parameter: $ =>
      seq(
        $.identifier,
        optional("comptime"),
        $.identifier,
        ":",
        $._expression,
        optional(","),
      ),

    block: $ => seq("{", repeat($._statement), "}"),

    _statement: $ =>
      choice(
        $.variable_definition,
        $._expression,
        $.if_statement,
        $.while_loop,
        $.until_loop,
        $.repeat_loop,
        $.forever_loop,
        $.for_loop,
        $.return_statement,
      ),

    variable_definition: $ => seq("let", $.identifier, "=", $._expression),

    if_statement: $ =>
      seq(
        "if",
        $._expression,
        $.block,
        optional(seq("else", choice($.if_statement, $.block))),
      ),

    while_loop: $ => seq("while", $._expression, $.block),
    until_loop: $ => seq("until", $._expression, $.block),
    repeat_loop: $ => seq("repeat", $._expression, $.block),
    forever_loop: $ => seq("forever", $.block),
    for_loop: $ => seq("for", $.identifier, $._expression, $.block),

    return_statement: $ => seq("return", $._expression),

    _expression: $ =>
      choice(
        $.string_literal,
        $.number_literal,
        $.list_literal,
        "false",
        "true",
        $.function_call,
        $.generic_type_instantiation,
        $.reference,
        $.parenthesized_expression,
        $.named_argument,
        $.binary_expression,
        $.identifier,
      ),

    list_literal: $ => seq("[", repeat(seq($._expression, optional(","))), "]"),

    function_call: $ => seq($.identifier, $.arguments),

    arguments: $ => seq(token.immediate("("), repeat($._expression), ")"),

    generic_type_instantiation: $ =>
      prec(7, seq($._expression, $.type_parameters)),

    type_parameters: $ => seq(token.immediate("["), repeat($._expression), "]"),

    reference: $ => prec(6, seq("&", $._expression)),

    parenthesized_expression: $ => seq("(", $._expression, ")"),

    named_argument: $ => seq($.identifier, token.immediate(":"), $._expression),

    binary_expression: $ =>
      choice(
        prec.left(1, seq($._expression, "=", $._expression)),
        prec.left(2, seq($._expression, choice("<", "==", ">"), $._expression)),
        prec.left(3, seq($._expression, choice("+", "-"), $._expression)),
        prec.left(4, seq($._expression, choice("*", "/", "%"), $._expression)),
        prec.left(5, seq($._expression, "as", $._expression)),
        prec.left(8, seq($._expression, ".", $._expression)),
      ),

    identifier: $ => /[\p{XID_Start}_][\p{XID_Continue}-]*/,

    number_literal: $ =>
      choice(
        /[+-]?0[bB][01]+/,
        /[+-]?0[oO][0-7]+/,
        /[+-]?0[xX][0-9a-fA-F]+/,
        /[+-]?[0-9]+(\.[0-9]+)?([eE][+-]?[0-9]+)?/,
      ),

    string_literal: $ => /"[^"\n]*"?/,

    comment: $ => /#.*/,
  },
});

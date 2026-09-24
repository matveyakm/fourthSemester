// <copyright file="LambdaParser.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

/// <summary>
/// FParsec-based syntactic analyzer for the lambda calculus language:
/// named definitions, multi-parameter abstractions and applications.
/// </summary>
module LambdaParser.Parser

open FParsec
open LambdaParser.LambdaExpression

/// <summary>
/// In-line whitespace is a sequence of spaces or tabs. Newlines do not belong
/// to this set: they terminate a program item (a definition or an expression).
/// </summary>
let private horizontalWhitespace =
    skipManySatisfy (fun character -> character = ' ' || character = '\t')

/// <summary>
/// A separator between program items is one or more newlines. A blank line,
/// being a sequence of newlines, is therefore skipped silently.
/// </summary>
let private itemSeparator =
    horizontalWhitespace
    >>. skipMany1 (choice [ pchar '\n'; pchar '\r' ])
    .>> horizontalWhitespace

/// <summary>
/// Predicate for the first character of an identifier: a latin letter.
/// </summary>
let private isIdentifierStart character =
    System.Char.IsAsciiLetter character

/// <summary>
/// Predicate for a subsequent character of an identifier: latin letter or digit.
/// </summary>
let private isIdentifierPart character =
    isIdentifierStart character || System.Char.IsAsciiDigit character

/// <summary>
/// Parses an identifier, rejecting the reserved keyword "let".
/// </summary>
let private identifierParser : Parser<string, unit> =
    many1Satisfy2 isIdentifierStart isIdentifierPart
    >>= fun name ->
        if name = "let" then
            fail "the word 'let' is a reserved keyword and cannot name a variable"
        else
            preturn name

/// <summary>
/// Parses an identifier followed by optional in-line whitespace.
/// </summary>
let private identifierToken =
    identifierParser .>> horizontalWhitespace

/// <summary>
/// Parses the reserved keyword "let" followed by optional whitespace.
/// The keyword must not be a prefix of a longer identifier.
/// </summary>
let private keywordLetParser =
    pstring "let"
    .>> notFollowedBy (satisfy isIdentifierPart)
    .>> horizontalWhitespace

/// <summary>
/// Forwarded parser for a lambda expression; its body is bound in the
/// parenthesized and abstraction parsers which recursively refer to expressions.
/// </summary>
let private expressionParser, expressionParserRef = createParserForwardedToRef ()

/// <summary>
/// Parses a lambda abstraction with one or more bound variables,
/// e.g. \x y . body. Several variables are desugared into nested abstractions.
/// The body does not cross a line break, which marks the end of an item.
/// </summary>
let private abstractionParser =
    pchar '\\' .>> horizontalWhitespace
    >>. many1 identifierToken
    .>> pchar '.' .>> horizontalWhitespace
    .>>. expressionParser
    |>> fun (variables, body) ->
        List.foldBack (fun variable accumulator -> Abstraction (variable, accumulator)) variables body

/// <summary>
/// Parses a parenthesized expression, "( <expression> )", with in-line whitespace around it.
/// </summary>
let private parenthesizedParser =
    between
        (pchar '(' .>> horizontalWhitespace)
        (horizontalWhitespace .>> pchar ')')
        expressionParser

/// <summary>
/// Parses an atomic argument: an identifier or a parenthesized expression.
/// </summary>
let private argumentParser =
    (choice
        [ identifierParser |>> Variable
          parenthesizedParser ])
    .>> horizontalWhitespace

/// <summary>
/// Parses an argument of an application: an atomic argument or a lambda abstraction.
/// An abstraction is allowed only as the final argument of an application.
/// </summary>
let private applicationArgumentParser =
    attempt argumentParser
    <|> abstractionParser

/// <summary>
/// Parses an application: the first argument followed by zero or more arguments.
/// The application is built as a left-associated chain of nodes.
/// </summary>
let private applicationParser =
    argumentParser >>= fun firstArgument ->
        many (attempt applicationArgumentParser)
        |>> fun remainingArguments ->
            List.fold
                (fun accumulator argument -> Application (accumulator, argument))
                firstArgument
                remainingArguments

/// <summary>
/// Parses a lambda expression: an abstraction, an application or a bare argument.
/// </summary>
let private expressionBodyParser =
    choice
        [ abstractionParser
          applicationParser ]

do
    expressionParserRef := expressionBodyParser

/// <summary>
/// Parses a named definition, "let <identifier> = <expression>".
/// </summary>
let private definitionParser =
    keywordLetParser
    >>. identifierToken
    >>= fun name ->
        pchar '=' .>> horizontalWhitespace
        >>. expressionParser
        |>> fun body -> DefinitionItem { Name = name; Body = body }

/// <summary>
/// Parses a single item of a program: a named definition or a lambda expression.
/// The reserved keyword "let" unambiguously marks a definition.
/// </summary>
let private itemParser =
    choice
        [ attempt definitionParser
          expressionParser |>> ExpressionItem ]

/// <summary>
/// Parses a whole program: a non-empty sequence of items, each separated from
/// the next by a line break, followed by end of input. Leading, trailing and
/// stray separator characters are permitted.
/// </summary>
let private programParser =
    horizontalWhitespace
    >>. many itemSeparator
    >>. itemParser
    >>= fun firstItem ->
        many (attempt (itemSeparator >>. itemParser))
        |>> fun remainingItems -> firstItem :: remainingItems
    .>> many itemSeparator
    .>> horizontalWhitespace
    .>> eof

/// <summary>
/// Parses a whole program from a string. Every "let" definition is a separate
/// program item; definitions and expressions are separated by line breaks or
/// line breaks and may be interleaved arbitrarily.
/// </summary>
/// <param name="inputText">The source text of the program.</param>
/// <returns>FParsec result carrying the list of parsed program items.</returns>
let parseProgram inputText =
    run programParser inputText

/// <summary>
/// Parses a single lambda expression from a string.
/// </summary>
/// <param name="inputText">The source text of the expression.</param>
/// <returns>FParsec result carrying the parsed expression.</returns>
let parseExpression inputText =
    run (horizontalWhitespace >>. expressionParser .>> eof) inputText

/// <summary>
/// Parses a whole program and folds the FParsec result into an F# Result value.
/// </summary>
/// <param name="inputText">The source text of the program.</param>
/// <returns>Ok with the parsed program, or Error with the parser diagnostics.</returns>
let tryParseProgram inputText : Result<Program, string> =
    match parseProgram inputText with
    | Success (items, _, _) -> Result.Ok { Items = items }
    | Failure (message, _, _) -> Result.Error message

/// <summary>
/// Parses a single lambda expression and folds the FParsec result into an F# Result value.
/// </summary>
/// <param name="inputText">The source text of the expression.</param>
/// <returns>Ok with the parsed expression, or Error with the parser diagnostics.</returns>
let tryParseExpression inputText : Result<LambdaExpression, string> =
    match parseExpression inputText with
    | Success (expression, _, _) -> Result.Ok expression
    | Failure (message, _, _) -> Result.Error message
// <copyright file="LambdaExpression.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

/// <summary>
/// Abstract syntax tree of the lambda calculus language with named definitions,
/// together with a pretty printer that can emit parseable text.
/// </summary>
module LambdaParser.LambdaExpression

/// <summary>
/// Lambda expression represented as an abstract syntax tree.
/// A multi-parameter abstraction is desugared into nested single-parameter ones.
/// </summary>
type LambdaExpression =
    | Variable of string
    | Abstraction of string * LambdaExpression
    | Application of LambdaExpression * LambdaExpression

/// <summary>
/// Named definition that binds a name to a lambda expression.
/// </summary>
type Definition =
    { Name: string
      Body: LambdaExpression }

/// <summary>
/// A single item of a program: either a named definition or a lambda expression.
/// </summary>
type ProgramItem =
    | DefinitionItem of Definition
    | ExpressionItem of LambdaExpression

/// <summary>
/// The result of parsing a whole program: source-ordered collection of items.
/// </summary>
type Program =
    { Items: ProgramItem list }

/// <summary>
/// Prints a lambda expression as text that follows the grammar of the language.
/// The printed text can be parsed back into an alpha-equivalent expression.
/// </summary>
/// <param name="expression">The lambda expression to print.</param>
/// <returns>The textual representation of the expression.</returns>
let rec toText (expression: LambdaExpression) : string =
    match expression with
    | Variable name -> name
    | Abstraction (variable, body) -> sprintf "\\%s.%s" variable (toText body)
    | Application (functionExpression, argumentExpression) ->
        sprintf "%s %s"
            (toTextFunction functionExpression)
            (toTextArgument argumentExpression)

/// <summary>
/// Prints the function part of an application, parenthesizing a lambda abstraction.
/// </summary>
and toTextFunction expression =
    match expression with
    | Abstraction _ -> sprintf "(%s)" (toText expression)
    | _ -> toText expression

/// <summary>
/// Prints an argument of an application, parenthesizing non-atomic arguments.
/// </summary>
and toTextArgument expression =
    match expression with
    | Variable _ -> toText expression
    | _ -> sprintf "(%s)" (toText expression)
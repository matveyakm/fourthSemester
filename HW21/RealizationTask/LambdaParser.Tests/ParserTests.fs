// <copyright file="ParserTests.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

module LambdaParserTests.ParserTests

open NUnit.Framework
open FsUnit
open LambdaParser.LambdaExpression

/// <summary>
/// Builds a variable expression with the given name.
/// </summary>
let private v name = Variable name

/// <summary>
/// Builds a lambda abstraction with the given bound variable and body.
/// </summary>
let private lam bound body = Abstraction (bound, body)

/// <summary>
/// Builds an application of the given function to the given argument.
/// </summary>
let private app func arg = Application (func, arg)

/// <summary>
/// Wraps a value into a Result carrying a string error, matching the library API.
/// </summary>
let private ok value : Result<'a, string> = Result.Ok value

/// <summary>
/// A single identifier parses as a variable.
/// </summary>
[<Test>]
let ``single identifier parses as a variable`` () =
    LambdaParser.Parser.tryParseExpression "x" |> should equal (ok (v "x"))

/// <summary>
/// An identifier may contain digits.
/// </summary>
[<Test>]
let ``identifier with digits parses as a variable`` () =
    LambdaParser.Parser.tryParseExpression "x1" |> should equal (ok (v "x1"))

/// <summary>
/// Several atoms form a left-associated application.
/// </summary>
[<Test>]
let ``several atoms form a left-associated application`` () =
    LambdaParser.Parser.tryParseExpression "x y z"
    |> should equal (ok (app (app (v "x") (v "y")) (v "z")))

/// <summary>
/// A single lambda abstraction with one parameter parses correctly.
/// </summary>
[<Test>]
let ``single abstraction parses correctly`` () =
    LambdaParser.Parser.tryParseExpression """\x.x"""
    |> should equal (ok (lam "x" (v "x")))

/// <summary>
/// A multi-parameter abstraction desugars into nested abstractions.
/// </summary>
[<Test>]
let ``multi-parameter abstraction desugars into nested abstractions`` () =
    LambdaParser.Parser.tryParseExpression """\x y.x"""
    |> should equal (ok (lam "x" (lam "y" (v "x"))))

/// <summary>
/// The body of an abstraction is the whole rest of the term.
/// </summary>
[<Test>]
let ``abstraction body extends to the end of the term`` () =
    LambdaParser.Parser.tryParseExpression """\x.y x"""
    |> should equal (ok (lam "x" (app (v "y") (v "x"))))

/// <summary>
/// Parentheses force the grouping of an application inside an application.
/// </summary>
[<Test>]
let ``parentheses group the right argument`` () =
    LambdaParser.Parser.tryParseExpression "x (y z)"
    |> should equal (ok (app (v "x") (app (v "y") (v "z"))))

/// <summary>
/// A lambda abstraction without parentheses is accepted as the final argument.
/// </summary>
[<Test>]
let ``abstraction is accepted as the final argument`` () =
    LambdaParser.Parser.tryParseExpression """x \y.y"""
    |> should equal (ok (app (v "x") (lam "y" (v "y"))))

/// <summary>
/// An abstraction inside a body may absorb the following lambda argument.
/// </summary>
[<Test>]
let ``tailing abstraction inside an abstraction body parses correctly`` () =
    LambdaParser.Parser.tryParseExpression """\x. y x \z.z"""
    |> should equal
        (ok
            (lam "x" (app (app (v "y") (v "x")) (lam "z" (v "z")))))

/// <summary>
/// A parenthesized abstraction applies to the following argument.
/// </summary>
[<Test>]
let ``parenthesized abstraction applies to the next atom`` () =
    LambdaParser.Parser.tryParseExpression """(\x.x) y"""
    |> should equal (ok (app (lam "x" (v "x")) (v "y")))

/// <summary>
/// The reserved word "let" alone is not a valid expression.
/// </summary>
[<Test>]
let ``reserved keyword let is rejected as an expression`` () =
    LambdaParser.Parser.tryParseExpression "let" |> Result.isError |> should equal true

/// <summary>
/// The reserved word "let" cannot bind a variable of an abstraction.
/// </summary>
[<Test>]
let ``reserved keyword let is rejected as a bound variable`` () =
    LambdaParser.Parser.tryParseExpression """\let.let""" |> Result.isError |> should equal true

/// <summary>
/// An identifier that merely starts with "let" is an ordinary variable.
/// </summary>
[<Test>]
let ``identifier starting with let is allowed`` () =
    LambdaParser.Parser.tryParseExpression "letx" |> should equal (ok (v "letx"))

/// <summary>
/// An abstraction without a body is a parse error.
/// </summary>
[<Test>]
let ``abstraction without a body is rejected`` () =
    LambdaParser.Parser.tryParseExpression """\x.""" |> Result.isError |> should equal true

/// <summary>
/// An unclosed parenthesis is a parse error.
/// </summary>
[<Test>]
let ``unclosed parenthesis is rejected`` () =
    LambdaParser.Parser.tryParseExpression "(" |> Result.isError |> should equal true

/// <summary>
/// A stray closing parenthesis is a parse error.
/// </summary>
[<Test>]
let ``stray closing parenthesis is rejected`` () =
    LambdaParser.Parser.tryParseExpression "x )" |> Result.isError |> should equal true

/// <summary>
/// A program with a definition and a final expression parses into two items.
/// </summary>
[<Test>]
let ``program with definition and expression parses into two items`` () =
    let expected =
        { Items =
            [ DefinitionItem { Name = "K"; Body = lam "x" (lam "y" (v "x")) }
              ExpressionItem (app (app (v "S") (v "K")) (v "K")) ] }
    LambdaParser.Parser.tryParseProgram "let K = \\x y.x\nS K K"
    |> should equal (ok expected)

/// <summary>
/// Definitions and expressions may alternate freely in a program.
/// </summary>
[<Test>]
let ``definitions and expressions may alternate in a program`` () =
    let expected =
        { Items =
            [ DefinitionItem
                { Name = "I"; Body = lam "x" (v "x") }
              ExpressionItem (app (v "I") (v "y"))
              DefinitionItem
                { Name = "t"; Body = app (app (v "a") (v "b")) (v "c") } ] }
    LambdaParser.Parser.tryParseProgram "let I = \\x.x\nI y\nlet t = a b c"
    |> should equal (ok expected)

/// <summary>
/// A program of several definitions only is a valid parse result.
/// </summary>
[<Test>]
let ``a program of definitions only parses`` () =
    let programText =
        """let S = \x y z.x z (y z)
let K = \x y.x"""
    LambdaParser.Parser.tryParseProgram programText
    |> Result.map (fun program -> program.Items.Length)
    |> should equal (ok 2)

/// <summary>
/// An empty program is rejected since the grammar has no epsilon transitions.
/// </summary>
[<Test>]
let ``empty program is rejected`` () =
    LambdaParser.Parser.tryParseProgram "   " |> Result.isError |> should equal true
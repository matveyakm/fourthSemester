// <copyright file="LambdaExpressionTests.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

module LambdaParserTests.LambdaExpressionTests

open NUnit.Framework
open FsUnit
open LambdaParser.LambdaExpression
open LambdaParser.Parser

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
/// A variable prints as its name.
/// </summary>
[<Test>]
let ``toText of a variable is its name`` () =
    toText (v "x") |> should equal "x"

/// <summary>
/// A lambda abstraction prints the backslash, the bound variable and the body.
/// </summary>
[<Test>]
let ``toText of a lambda abstraction uses slash dot notation`` () =
    toText (lam "x" (app (v "x") (v "y"))) |> should equal """\x.x y"""

/// <summary>
/// A left-associated application prints its atoms separated by spaces.
/// </summary>
[<Test>]
let ``toText of a left-associated application keeps no parentheses`` () =
    toText (app (app (v "S") (v "K")) (v "K")) |> should equal "S K K"

/// <summary>
/// An application used as an argument is parenthesized when printed.
/// </summary>
[<Test>]
let ``toText parenthesizes an application in argument position`` () =
    toText (app (v "x") (app (v "y") (v "z"))) |> should equal "x (y z)"

/// <summary>
/// A lambda abstraction used as an argument is parenthesized when printed.
/// </summary>
[<Test>]
let ``toText parenthesizes a lambda in argument position`` () =
    toText (app (v "x") (lam "y" (v "y"))) |> should equal "x (\y.y)"

/// <summary>
/// The printed text of an abstraction body is reparsed to an equal expression.
/// </summary>
[<Test>]
let ``abstraction round-trips through toText`` () =
    let expression = lam "x" (app (v "x") (lam "y" (v "y")))
    tryParseExpression (toText expression) |> should equal (ok expression)

/// <summary>
/// The printed text of an application is reparsed to an equal expression.
/// </summary>
[<Test>]
let ``application round-trips through toText`` () =
    let expression =
        app
            (app (v "f") (app (v "x") (v "y")))
            (lam "z" (v "z"))
    tryParseExpression (toText expression) |> should equal (ok expression)

/// <summary>
/// A deeply nested abstraction round-trips through its printed text.
/// </summary>
[<Test>]
let ``nested abstraction round-trips through toText`` () =
    let expression = lam "a" (lam "b" (app (v "a") (v "b")))
    tryParseExpression (toText expression) |> should equal (ok expression)
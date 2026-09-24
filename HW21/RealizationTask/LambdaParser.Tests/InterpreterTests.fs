// <copyright file="InterpreterTests.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

module LambdaParserTests.InterpreterTests

open NUnit.Framework
open FsUnit
open LambdaParser.Interpreter

/// <summary>
/// Wraps a value into a Result carrying a string error, matching the library API.
/// </summary>
let private ok value : Result<'a, string> = Result.Ok value

/// <summary>
/// Wraps a message into an Error of a Result, matching the library API.
/// </summary>
let private err message : Result<'a, string> = Result.Error message

/// <summary>
/// The canonical example from the task: S K K reduces to the identity.
/// </summary>
[<Test>]
let ``S K K reduces to the identity`` () =
    let program =
        """let S = \x y z.x z (y z)
let K = \x y.x
S K K"""
    interpret program |> should equal (ok """\x.x""")

/// <summary>
/// A simple definition followed by an expression reduces to the body.
/// </summary>
[<Test>]
let ``defined identity applied to a variable`` () =
    interpret "let I = \\x.x\nI y" |> should equal (ok "y")

/// <summary>
/// One definition may reference another one.
/// </summary>
[<Test>]
let ``a definition may reference another definition`` () =
    let program =
        """let K = \x y.x
let I = K y
I a"""
    interpret program |> should equal (ok "y")

/// <summary>
/// Definitions and expressions can be interleaved; the last expression wins.
/// </summary>
[<Test>]
let ``last expression is reduced with all preceding definitions`` () =
    let program =
        """let I = \x.x
I y
let t = a b c
I z"""
    interpret program |> should equal (ok "z")

/// <summary>
/// Terms with undefined names stay unchanged.
/// </summary>
[<Test>]
let ``undefined names stay unchanged`` () =
    interpret "S K K" |> should equal (ok "S K K")

/// <summary>
/// The output is alpha-canonical, so the identity printed as "\x.x" everywhere.
/// </summary>
[<Test>]
let ``alpha-equivalent inputs produce the same canonical output`` () =
    interpret """\y.y""" |> should equal (ok """\x.x""")
    interpret """\z.z""" |> should equal (ok """\x.x""")

/// <summary>
/// An abstraction with a free variable keeps that variable in the output.
/// </summary>
[<Test>]
let ``free variables are preserved in the output`` () =
    interpret """\y.z""" |> should equal (ok """\x.z""")

/// <summary>
/// The reserved keyword "let" cannot appear inside an expression.
/// </summary>
[<Test>]
let ``reserved keyword yields an error`` () =
    interpret """\let.let""" |> Result.isError |> should equal true

/// <summary>
/// A program without a single expression cannot be reduced.
/// </summary>
[<Test>]
let ``program without an expression yields an error`` () =
    let expected : Result<string, string> =
        err "a program must contain at least one lambda expression to reduce"
    interpret "let K = \\x y.x" |> should equal expected

/// <summary>
/// An empty program is a parse error.
/// </summary>
[<Test>]
let ``empty program yields an error`` () =
    interpret "" |> Result.isError |> should equal true

/// <summary>
/// A cyclic definition is broken by leaving the name unresolved.
/// </summary>
[<Test>]
let ``cyclic definition leaves the name unresolved`` () =
    interpret "let x = x\nx" |> should equal (ok "x")

/// <summary>
/// A program stored in a file is interpreted like the same text given as a string.
/// </summary>
[<Test>]
let ``file input is interpreted like string input`` () =
    let filePath = System.IO.Path.GetTempFileName()
    try
        System.IO.File.WriteAllText(
            filePath,
            """let S = \x y z.x z (y z)
let K = \x y.x
S K K""")
        interpretFile filePath |> should equal (ok """\x.x""")
    finally
        System.IO.File.Delete filePath

/// <summary>
/// A missing file is reported as an error rather than an exception.
/// </summary>
[<Test>]
let ``missing file yields an error`` () =
    let missingPath = System.IO.Path.Combine(System.IO.Path.GetTempPath(), "no-such-file.lambda")
    interpretFile missingPath |> Result.isError |> should equal true

/// <summary>
/// Multi-parameter abstractions survive the full pipeline.
/// </summary>
[<Test>]
let ``multi-parameter abstraction reduces fully`` () =
    interpret """(\x y.x) a b""" |> should equal (ok "a")
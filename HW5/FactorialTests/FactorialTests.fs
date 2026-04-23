// <copyright file="FactorialTests.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

module FactorialTests

open NUnit.Framework
open FsUnit

/// <summary>
/// Tests for factorial function - basic cases
/// </summary>
[<Test>]
let ``factorial of 0 should be 1`` () =
    Factorial.factorial 0 |> should equal (Some 1L)

[<Test>]
let ``factorial of 1 should be 1`` () =
    Factorial.factorial 1 |> should equal (Some 1L)

/// <summary>
/// Tests for factorial function - positive cases
/// </summary>
[<Test>]
let ``factorial of 5 should be 120`` () =
    Factorial.factorial 5 |> should equal (Some 120L)

[<Test>]
let ``factorial of 10 should be 3628800`` () =
    Factorial.factorial 10 |> should equal (Some 3628800L)

/// <summary>
/// Tests for factorial function - negative integer handling
/// </summary>
[<Test>]
let ``factorial of negative integer should return None`` () =
    Factorial.factorial -1 |> should equal None

[<Test>]
let ``factorial of -5 should return None`` () =
    Factorial.factorial -5 |> should equal None

/// <summary>
/// Tests for factorial function - larger values
/// </summary>
[<Test>]
let ``factorial of 20 should be correct`` () =
    Factorial.factorial 20 |> should equal (Some 2432902008176640000L)
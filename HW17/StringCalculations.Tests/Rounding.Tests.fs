// <copyright file="Rounding.Tests.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

module MathWorkflows.Tests.Rounding

open NUnit.Framework
open FsUnit
open MathWorkflows
open MathWorkflows.Rounding

/// <summary>
/// Tests for the rounding workflow with precision 3 as specified in the requirements.
/// </summary>
[<Test>]
let ``rounding with precision 3 as in spec`` () =
    let result = rounding 3 {
        let! a = 2.0 / 12.0
        let! b = 3.5
        return a / b
    }
    result |> should equal 0.048

/// <summary>
/// Tests that rounding with precision 0 rounds to the nearest integer.
/// </summary>
[<Test>]
let ``rounding with precision 0 rounds to integer`` () =
    let result = rounding 0 {
        let! a = 1.7
        let! b = 2.3
        return a + b
    }
    result |> should equal 4.0

/// <summary>
/// Tests that intermediate rounding affects the final result.
/// </summary>
[<Test>]
let ``rounding intermediate result affects final result`` () =
    let result = rounding 1 {
        let! a = 1.44
        let! b = 2.17
        return a + b
    }
    result |> should equal 3.6

/// <summary>
/// Tests that ReturnFrom works correctly (passes through already rounded values).
/// </summary>
[<Test>]
let ``rounding ReturnFrom passes through values`` () =
    let result = rounding 2 {
        return! 3.14159
    }
    result |> should equal 3.14

/// <summary>
/// Tests rounding with negative numbers.
/// </summary>
[<Test>]
let ``rounding handles negative numbers correctly`` () =
    let result = rounding 2 {
        let! a = -1.234
        let! b = -2.567
        return a + b
    }
    result |> should equal -3.8

/// <summary>
/// Tests that higher precision preserves more decimal places.
/// </summary>
[<Test>]
let ``rounding with higher precision preserves more decimals`` () =
    let result = rounding 5 {
        let! a = 1.0 / 3.0
        return a
    }
    result |> should equal 0.33333
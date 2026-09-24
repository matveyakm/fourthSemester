// <copyright file="StringCalculation.Tests.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

module MathWorkflows.Tests.StringCalculation

open System
open NUnit.Framework
open FsUnit
open MathWorkflows
open MathWorkflows.StringCalculation

/// <summary>
/// Tests that calculate workflow returns Some sum for valid numeric strings.
/// </summary>
[<Test>]
let ``calculate with valid numbers returns Some sum`` () =
    let result = calculate {
        let! x = "1"
        let! y = "2"
        let z = x + y
        return z
    }
    result |> should equal (Some 3.0)

/// <summary>
/// Tests that calculate workflow returns None when any input is invalid.
/// </summary>
[<Test>]
let ``calculate with invalid number returns None`` () =
    let result = calculate {
        let! x = "1"
        let! y = "\u042a"
        let z = x + y
        return z
    }
    result |> should equal None

/// <summary>
/// Tests that calculate workflow returns None when all inputs are invalid.
/// </summary>
[<Test>]
let ``calculate with all invalid returns None`` () =
    let result = calculate {
        let! x = "abc"
        let! y = "def"
        return x + y
    }
    result |> should equal None

/// <summary>
/// Tests that calculate workflow returns None when mixing valid and invalid inputs.
/// </summary>
[<Test>]
let ``calculate with mixed valid and invalid returns None`` () =
    let result = calculate {
        let! x = "3.1415"
        let! y = "banana"
        return x + y
    }
    result |> should equal None

/// <summary>
/// Tests that calculate workflow supports ReturnFrom for composing computations.
/// </summary>
[<Test>]
let ``calculate supports ReturnFrom`` () =
    let step s: float option =
        calculate {
            let! x = s
            return x
        }
    let result = calculate {
        return! step "67"
    }
    result |> should equal (Some 67.0)

/// <summary>
/// Tests that calculate workflow handles decimal numbers in strings.
/// </summary>
[<Test>]
let ``calculate handles decimal strings correctly`` () =
    let result = calculate {
        let! x = "5.5"
        let! y = "2.5"
        return x + y
    }
    result |> should equal (Some 8.0)

/// <summary>
/// Tests that calculate workflow handles negative numbers in strings.
/// </summary>
[<Test>]
let ``calculate handles negative number strings correctly`` () =
    let result = calculate {
        let! x = "-10"
        let! y = "5"
        return x + y
    }
    result |> should equal (Some -5.0)

/// <summary>
/// Tests that calculate workflow handles scientific notation.
/// </summary>
[<Test>]
let ``calculate handles scientific notation`` () =
    let result = calculate {
        let! x = "1e2"
        let! y = "2e1"
        return x + y
    }
    result |> should equal (Some 120.0)

/// <summary>
/// Tests that calculate workflow handles NaN string (returns Some NaN).
/// </summary>
[<Test>]
let ``calculate handles NaN string`` () =
    let result = calculate {
        let! x = "NaN"
        let! y = "5"
        return x + y
    }
    result |> should equal (Some Double.NaN)

/// <summary>
/// Tests that calculate workflow handles Infinity string (returns Some Infinity).
/// </summary>
[<Test>]
let ``calculate handles Infinity string`` () =
    let result = calculate {
        let! x = "Infinity"
        let! y = "5"
        return x + y
    }
    result |> should equal (Some Double.PositiveInfinity)

/// <summary>
/// Tests that calculate workflow handles negative Infinity string (returns Some -Infinity).
/// </summary>
[<Test>]
let ``calculate handles negative Infinity string`` () =
    let result = calculate {
        let! x = "-Infinity"
        let! y = "5"
        return x + y
    }
    result |> should equal (Some Double.NegativeInfinity)

/// <summary>
/// Tests that calculate workflow returns None for empty string.
/// </summary>
[<Test>]
let ``calculate returns None for empty string`` () =
    let result = calculate {
        let! x = ""
        let! y = "5"
        return x + y
    }
    result |> should equal None
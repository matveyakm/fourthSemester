// <copyright file="ExpressionTreeTests.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

module ArithmeticParserTests.ExpressionTreeTests

open NUnit.Framework
open FsUnit
open ArithmeticParser.ExpressionTree

/// <summary>
/// Tests for evaluating simple number expressions.
/// </summary>
[<Test>]
let ``evaluate Number 5 should return 5`` () =
    /// <param name="expr">Number 5 expression</param>
    evaluate (Number 5) |> should equal 5

/// <summary>
/// Tests for evaluating zero.
/// </summary>
[<Test>]
let ``evaluate Number 0 should return 0`` () =
    /// <param name="expr">Number 0 expression</param>
    evaluate (Number 0) |> should equal 0

/// <summary>
/// Tests for evaluating negative numbers.
/// </summary>
[<Test>]
let ``evaluate Number -10 should return -10`` () =
    /// <param name="expr">Number -10 expression</param>
    evaluate (Number -10) |> should equal -10

/// <summary>
/// Tests for addition expressions.
/// </summary>
[<Test>]
let ``evaluate Add(3, 5) should return 8`` () =
    /// <param name="expr">Add expression 3 + 5</param>
    evaluate (Add (Number 3, Number 5)) |> should equal 8

/// <summary>
/// Tests for nested addition expressions.
/// </summary>
[<Test>]
let ``evaluate nested Add should return sum`` () =
    /// <param name="expr">Nested add expression (1 + 2) + 3</param>
    evaluate (Add (Add (Number 1, Number 2), Number 3)) |> should equal 6

/// <summary>
/// Tests for subtraction expressions.
/// </summary>
[<Test>]
let ``evaluate Sub(10, 3) should return 7`` () =
    /// <param name="expr">Sub expression 10 - 3</param>
    evaluate (Sub (Number 10, Number 3)) |> should equal 7

/// <summary>
/// Tests for subtraction resulting in negative value.
/// </summary>
[<Test>]
let ``evaluate Sub(5, 10) should return -5`` () =
    /// <param name="expr">Sub expression 5 - 10</param>
    evaluate (Sub (Number 5, Number 10)) |> should equal -5

/// <summary>
/// Tests for multiplication expressions.
/// </summary>
[<Test>]
let ``evaluate Mul(4, 5) should return 20`` () =
    /// <param name="expr">Mul expression 4 * 5</param>
    evaluate (Mul (Number 4, Number 5)) |> should equal 20

/// <summary>
/// Tests for nested multiplication expressions.
/// </summary>
[<Test>]
let ``evaluate nested Mul should return product`` () =
    /// <param name="expr">Nested mul expression (2 * 3) * 4</param>
    evaluate (Mul (Mul (Number 2, Number 3), Number 4)) |> should equal 24

/// <summary>
/// Tests for division expressions.
/// </summary>
[<Test>]
let ``evaluate Div(20, 4) should return 5`` () =
    /// <param name="expr">Div expression 20 / 4</param>
    evaluate (Div (Number 20, Number 4)) |> should equal 5

/// <summary>
/// Tests for integer division.
/// </summary>
[<Test>]
let ``evaluate Div(7, 2) should return 3 (integer division)`` () =
    /// <param name="expr">Div expression 7 / 2</param>
    evaluate (Div (Number 7, Number 2)) |> should equal 3

/// <summary>
/// Tests for complex expressions with mixed operators.
/// </summary>
[<Test>]
let ``evaluate (2 + 3) * 4 should return 20`` () =
    /// <param name="expr">Complex expression (2 + 3) * 4</param>
    evaluate (Mul (Add (Number 2, Number 3), Number 4)) |> should equal 20

/// <summary>
/// Tests for subtraction and multiplication combined.
/// </summary>
[<Test>]
let ``evaluate (10 - 2) * 3 should return 24`` () =
    /// <param name="expr">Complex expression (10 - 2) * 3</param>
    evaluate (Mul (Sub (Number 10, Number 2), Number 3)) |> should equal 24

/// <summary>
/// Tests for operator precedence without parentheses.
/// </summary>
[<Test>]
let ``evaluate 10 - 2 * 3 should return 4 (without parentheses, left-to-right)`` () =
    /// <param name="expr">Expression 10 - (2 * 3)</param>
    evaluate (Sub (Number 10, Mul (Number 2, Number 3))) |> should equal 4

/// <summary>
/// Tests for addition and division combined.
/// </summary>
[<Test>]
let ``evaluate (10 + 5) / 3 should return 5`` () =
    /// <param name="expr">Complex expression (10 + 5) / 3</param>
    evaluate (Div (Add (Number 10, Number 5), Number 3)) |> should equal 5

/// <summary>
/// Tests for fully complex expression.
/// </summary>
[<Test>]
let ``evaluate complex: ((2 + 3) * (4 - 1)) / 5 should return 3`` () =
    /// <param name="expr">Fully complex expression ((2 + 3) * (4 - 1)) / 5</param>
    evaluate (Div (Mul (Add (Number 2, Number 3), Sub (Number 4, Number 1)), Number 5)) |> should equal 3

/// <summary>
/// Tests for division by zero handling.
/// </summary>
[<Test>]
let ``evaluate Div(10, 0) should raise exception`` () =
    /// <param name="expr">Division by zero expression</param>
    (fun () -> evaluate (Div (Number 10, Number 0)) |> ignore)
    |> should throw typeof<System.Exception>

/// <summary>
/// Tests for tail-recursive division by zero handling.
/// </summary>
[<Test>]
let ``evaluateTailRecursive Div(10, 0) should raise exception`` () =
    /// <param name="expr">Tail recursive division by zero</param>
    (fun () -> evaluateTailRecursive (Div (Number 10, Number 0)) |> ignore)
    |> should throw typeof<System.Exception>

/// <summary>
/// Tests for tail-recursive evaluation with simple number.
/// </summary>
[<Test>]
let ``evaluateTailRecursive Number 42 should return 42`` () =
    /// <param name="expr">Tail recursive Number 42</param>
    evaluateTailRecursive (Number 42) |> should equal 42

/// <summary>
/// Tests for tail-recursive evaluation with addition.
/// </summary>
[<Test>]
let ``evaluateTailRecursive Add(5, 3) should return 8`` () =
    /// <param name="expr">Tail recursive Add(5, 3)</param>
    evaluateTailRecursive (Add (Number 5, Number 3)) |> should equal 8

/// <summary>
/// Tests for tail-recursive evaluation with subtraction.
/// </summary>
[<Test>]
let ``evaluateTailRecursive Sub(10, 4) should return 6`` () =
    /// <param name="expr">Tail recursive Sub(10, 4)</param>
    evaluateTailRecursive (Sub (Number 10, Number 4)) |> should equal 6

/// <summary>
/// Tests for tail-recursive evaluation with multiplication.
/// </summary>
[<Test>]
let ``evaluateTailRecursive Mul(6, 7) should return 42`` () =
    /// <param name="expr">Tail recursive Mul(6, 7)</param>
    evaluateTailRecursive (Mul (Number 6, Number 7)) |> should equal 42

/// <summary>
/// Tests for tail-recursive evaluation with division.
/// </summary>
[<Test>]
let ``evaluateTailRecursive Div(20, 4) should return 5`` () =
    /// <param name="expr">Tail recursive Div(20, 4)</param>
    evaluateTailRecursive (Div (Number 20, Number 4)) |> should equal 5

/// <summary>
/// Tests for tail-recursive evaluation with complex expression.
/// </summary>
[<Test>]
let ``evaluateTailRecursive complex: ((2 + 3) * (4 - 1)) + 10 should return 25`` () =
    /// <param name="expr">Tail recursive complex ((2 + 3) * (4 - 1)) + 10</param>
    evaluateTailRecursive (Add (Mul (Add (Number 2, Number 3), Sub (Number 4, Number 1)), Number 10)) |> should equal 25
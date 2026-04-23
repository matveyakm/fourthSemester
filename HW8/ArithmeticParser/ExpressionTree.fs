// <copyright file="ExpressionTree.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

module ArithmeticParser.ExpressionTree

/// <summary>
/// Represents an arithmetic expression as a parse tree using discriminated unions.
/// </summary>
type Expression =
    | Number of int
    | Add of Expression * Expression
    | Sub of Expression * Expression
    | Mul of Expression * Expression
    | Div of Expression * Expression

/// <summary>
/// Evaluates the given expression tree and returns the result.
/// Uses tail recursion for evaluation.
/// </summary>
/// <param name="expr">The expression tree to evaluate.</param>
/// <returns>The integer result of the expression evaluation.</returns>
let rec evaluate expr =
    match expr with
    | Number n -> n
    | Add (left, right) -> evaluate left + evaluate right
    | Sub (left, right) -> evaluate left - evaluate right
    | Mul (left, right) -> evaluate left * evaluate right
    | Div (left, right) ->
        let divisor = evaluate right
        if divisor = 0 then failwith "Division by zero"
        evaluate left / divisor

/// <summary>
/// Evaluates the expression using continuation-passing style for tail recursion.
/// </summary>
let evaluateTailRecursive expr =
    let rec evaluateK expr k =
        match expr with
        | Number n -> k n
        | Add (left, right) -> evaluateK left (fun leftVal -> evaluateK right (fun rightVal -> k (leftVal + rightVal)))
        | Sub (left, right) -> evaluateK left (fun leftVal -> evaluateK right (fun rightVal -> k (leftVal - rightVal)))
        | Mul (left, right) -> evaluateK left (fun leftVal -> evaluateK right (fun rightVal -> k (leftVal * rightVal)))
        | Div (left, right) -> evaluateK left (fun leftVal -> evaluateK right (fun rightVal -> 
            if rightVal = 0 then failwith "Division by zero"
            else k (leftVal / rightVal)))
    evaluateK expr id
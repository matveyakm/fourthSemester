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
/// Divides the given operands, returning None on division by zero.
/// </summary>
/// <param name="leftOperand">The result of the left operand evaluation.</param>
/// <param name="rightOperand">The result of the right operand evaluation.</param>
/// <returns>The quotient, or None if the divisor is zero.</returns>
let private safeDivide leftOperand rightOperand =
    match leftOperand, rightOperand with
    | Some leftValue, Some rightValue when rightValue <> 0 -> Some (leftValue / rightValue)
    | _ -> None

/// <summary>
/// Evaluates the given expression tree and returns the result.
/// Returns None if division by zero occurs.
/// </summary>
/// <param name="expr">The expression tree to evaluate.</param>
/// <returns>The result of the evaluation, or None if division by zero occurs.</returns>
let rec evaluate expr =
    match expr with
    | Number n -> Some n
    | Add (left, right) -> Option.map2 (+) (evaluate left) (evaluate right)
    | Sub (left, right) -> Option.map2 (-) (evaluate left) (evaluate right)
    | Mul (left, right) -> Option.map2 (*) (evaluate left) (evaluate right)
    | Div (left, right) -> safeDivide (evaluate left) (evaluate right)

/// <summary>
/// Evaluates the expression using continuation-passing style for tail recursion.
/// Returns None if division by zero occurs.
/// </summary>
/// <param name="expr">The expression tree to evaluate.</param>
/// <returns>The result of the evaluation, or None if division by zero occurs.</returns>
let evaluateTailRecursive expr =
    let rec evaluateK expr cont =
        match expr with
        | Number n -> cont (Some n)
        | Add (left, right) ->
            evaluateK left (fun leftValue ->
                evaluateK right (fun rightValue ->
                    cont (Option.map2 (+) leftValue rightValue)))
        | Sub (left, right) ->
            evaluateK left (fun leftValue ->
                evaluateK right (fun rightValue ->
                    cont (Option.map2 (-) leftValue rightValue)))
        | Mul (left, right) ->
            evaluateK left (fun leftValue ->
                evaluateK right (fun rightValue ->
                    cont (Option.map2 (*) leftValue rightValue)))
        | Div (left, right) ->
            evaluateK left (fun leftValue ->
                evaluateK right (fun rightValue ->
                    cont (safeDivide leftValue rightValue)))
    evaluateK expr id

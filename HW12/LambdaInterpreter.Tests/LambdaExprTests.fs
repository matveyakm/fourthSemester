// <copyright file="LambdaExprTests.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

module Tests

open NUnit.Framework
open FsUnit
open LambdaCalc

let private lam x body = LambdaExpr.Lam(x, body)
let private app func arg = LambdaExpr.App(func, arg)
let private v name = LambdaExpr.Var name

/// <summary>
/// Variable expression is a variable with the expected name
/// </summary>
[<Test>]
let createVariable () =
    match v "x" with
    | Var name -> name |> should equal "x"
    | _ -> Assert.Fail "Expected variable x"

/// <summary>
/// Lambda abstraction is built from the bound variable name and the body
/// </summary>
[<Test>]
let createLambdaAbstraction () =
    match lam "x" (v "x") with
    | Lam (name, body) ->
        name |> should equal "x"
        body |> should equal (v "x")
    | _ -> Assert.Fail "Expected lambda abstraction"

/// <summary>
/// Application is built from function and argument
/// </summary>
[<Test>]
let createApplication () =
    match app (v "f") (v "x") with
    | App (func, arg) ->
        func |> should equal (v "f")
        arg |> should equal (v "x")
    | _ -> Assert.Fail "Expected application"

/// <summary>
/// Free variables of variable
/// </summary>
[<Test>]
let freeVariablesOfVariable () = LambdaCalc.freeVariables (v "x") |> should equal (set ["x"])

/// <summary>
/// Free variables of lambda abstraction where bound variable is not free
/// </summary>
[<Test>]
let freeVariablesOfLambdaAbstraction () = 
    let result = LambdaCalc.freeVariables (lam "x" (v "x"))
    result.Count |> should equal 0

/// <summary>
/// Free variables in lambda with free variable in body
/// </summary>
[<Test>]
let freeVariablesInLambdaWithFreeVar () = LambdaCalc.freeVariables (lam "x" (v "y")) |> should equal (set ["y"])

/// <summary>
/// Free variables of application
/// </summary>
[<Test>]
let freeVariablesOfApplication () = LambdaCalc.freeVariables (app (lam "x" (v "x")) (v "y")) |> should equal (set ["y"])

/// <summary>
/// Free variables with nested shadowing
/// </summary>
[<Test>]
let freeVariablesWithNestedShadowing () = 
    let result = LambdaCalc.freeVariables (lam "x" (lam "x" (v "x")))
    result.Count |> should equal 0

/// <summary>
/// Beta reduce identity application
/// </summary>
[<Test>]
let betaReduceIdentityApplication () =
    let identity = lam "x" (v "x")
    LambdaCalc.betaReduce (app identity (v "y")) |> should equal (Some (v "y"))

/// <summary>
/// Beta reduce K combinator one step
/// </summary>
[<Test>]
let betaReduceKCombinatorOneStep () =
    let k = lam "x" (lam "y" (v "x"))
    let expr = app (app k (v "a")) (v "b")
    LambdaCalc.betaReduce expr |> should equal (Some (app (lam "y" (v "a")) (v "b")))

/// <summary>
/// Evaluate I I to normal form
/// </summary>
[<Test>]
let evaluateIIToNormalForm () =
    let identity = lam "x" (v "x")
    LambdaCalc.evaluate 100 (app identity identity) |> should equal (lam "x" (v "x"))

/// <summary>
/// Evaluate K combinator to normal form
/// </summary>
[<Test>]
let evaluateKCombinatorToNormalForm () =
    let k = lam "x" (lam "y" (v "x"))
    LambdaCalc.evaluate 100 (app (app k (v "a")) (v "b")) |> should equal (v "a")

/// <summary>
/// Evaluate normal order leftmost outermost
/// </summary>
[<Test>]
let evaluateNormalOrderLeftmostOuterMost () =
    let id = lam "x" (v "x")
    LambdaCalc.evaluate 100 (app id (app id (v "z"))) |> should equal (v "z")

/// <summary>
/// Beta reduce on variable
/// </summary>
[<Test>]
let betaReduceOnVariable () = LambdaCalc.betaReduce (v "x") |> should equal None

/// <summary>
/// Beta reduce on lambda with irreducible body
/// </summary>
[<Test>]
let betaReduceOnLambdaWithIrreducibleBody () = LambdaCalc.betaReduce (lam "x" (v "x")) |> should equal None

/// <summary>
/// Beta reduce on non-redex application
/// </summary>
[<Test>]
let betaReduceOnNonRedomApplication () = LambdaCalc.betaReduce (app (v "f") (v "x")) |> should equal None

/// <summary>
/// Substitution renames the bound variable to avoid capturing free variables of the replacement
/// </summary>
[<Test>]
let substitutionAvoidsCapture () =
    let expr = lam "x" (app (v "x") (v "y"))
    let replacement = v "x"
    let result = LambdaCalc.substitute expr "y" replacement
    result |> should equal (lam "x_1" (app (v "x_1") (v "x")))

/// <summary>
/// Beta reduction of (λx. λy. x y) y renames the bound y to avoid capture
/// </summary>
[<Test>]
let alphaConversionDuringBetaReduction () =
    let expr = app (lam "x" (lam "y" (app (v "x") (v "y")))) (v "y")
    let expected = lam "y_1" (app (v "y") (v "y_1"))
    LambdaCalc.betaReduce expr |> should equal (Some expected)
    LambdaCalc.evaluate 100 expr |> should equal expected

/// <summary>
/// Substitute variable not in body
/// </summary>
[<Test>]
let substituteVariableNotInBody () = LambdaCalc.substitute (lam "y" (v "y")) "x" (v "z") |> should equal (lam "y" (v "y"))

/// <summary>
/// Substitute with lambda expression
/// </summary>
[<Test>]
let substituteWithLambdaExpression () =
    let body = lam "z" (v "z")
    let replacement = lam "y" (v "y")
    let result = LambdaCalc.substitute body "z" replacement
    result |> should equal body

/// <summary>
/// Substitution with inner capture danger
/// </summary>
[<Test>]
let substitutionWithInnerCaptureDanger () =
    let body = lam "y" (lam "x" (v "y"))
    let replacement = v "x"
    let result = LambdaCalc.substitute body "y" replacement
    result |> should equal body

/// <summary>
/// Deeply nested substitution
/// </summary>
[<Test>]
let deeplyNestedSubstitution () =
    let expr = lam "y" (app (lam "x" (v "x")) (v "z"))
    let result = LambdaCalc.substitute expr "z" (v "w")
    match result with
    | Lam (y, App (Lam (x, Var xv), Var zv)) -> 
        zv |> should equal "w"
    | _ -> Assert.Fail "Unexpected result"

/// <summary>
/// Substitute at multiple occurrences
/// </summary>
[<Test>]
let substituteAtMultipleOccurrences () =
    let body = app (v "x") (v "x")
    let result = LambdaCalc.substitute body "x" (v "y")
    match result with
    | App (Var y1, Var y2) -> y1 |> should equal "y"; y2 |> should equal "y"
    | _ -> Assert.Fail "Expected substitution"

/// <summary>
/// Substitute in nested lambda
/// </summary>
[<Test>]
let substituteInNestedLambda () =
    let body = lam "x" (lam "y" (app (v "x") (v "y")))
    let result = LambdaCalc.substitute body "x" (v "z")
    result |> should equal body
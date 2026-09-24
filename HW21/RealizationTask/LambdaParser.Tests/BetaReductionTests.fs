// <copyright file="BetaReductionTests.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

module LambdaParserTests.BetaReductionTests

open NUnit.Framework
open FsUnit
open LambdaParser.LambdaExpression
open LambdaParser.BetaReduction

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
/// Free variables of a single variable contain that variable.
/// </summary>
[<Test>]
let ``free variables of a variable contain itself`` () =
    freeVariables (v "x") |> should equal (set [ "x" ])

/// <summary>
/// Free variables do not include the bound variable of an abstraction.
/// </summary>
[<Test>]
let ``free variables of an abstraction exclude its bound variable`` () =
    freeVariables (lam "x" (v "x")) |> Set.count |> should equal 0

/// <summary>
/// Free variables of a body without the bound variable are reported.
/// </summary>
[<Test>]
let ``free variables of an abstraction report the free body variable`` () =
    freeVariables (lam "x" (app (v "x") (v "y"))) |> should equal (set [ "y" ])

/// <summary>
/// Free variables of an application are the union of both sides.
/// </summary>
[<Test>]
let ``free variables of an application are the union`` () =
    freeVariables (app (v "f") (v "x")) |> should equal (set [ "f"; "x" ])

/// <summary>
/// Substitution replaces a free occurrence of the variable.
/// </summary>
[<Test>]
let ``substitution replaces the variable`` () =
    substitute (v "x") "x" (v "y") |> should equal (v "y")

/// <summary>
/// Substitution leaves a variable with a different name untouched.
/// </summary>
[<Test>]
let ``substitution leaves a different variable untouched`` () =
    substitute (v "z") "x" (v "y") |> should equal (v "z")

/// <summary>
/// Substitution descends into an application.
/// </summary>
[<Test>]
let ``substitution descends into an application`` () =
    substitute (app (v "x") (v "x")) "x" (v "y")
    |> should equal (app (v "y") (v "y"))

/// <summary>
/// Substitution renames a binder to avoid capturing free variables.
/// </summary>
[<Test>]
let ``substitution avoids capture by renaming the binder`` () =
    substitute (lam "x" (app (v "x") (v "y"))) "y" (v "x")
    |> should equal (lam "x_1" (app (v "x_1") (v "x")))

/// <summary>
/// Substitution does not touch a body whose variable does not occur free.
/// </summary>
[<Test>]
let ``substitution leaves an abstraction with no free occurrence untouched`` () =
    substitute (lam "x" (v "x")) "y" (v "x")
    |> should equal (lam "x" (v "x"))

/// <summary>
/// Beta reduction applies the identity function to its argument.
/// </summary>
[<Test>]
let ``beta reduction of identity application`` () =
    betaReduce (app (lam "x" (v "x")) (v "y"))
    |> should equal (Some (v "y"))

/// <summary>
/// A variable is already in normal form.
/// </summary>
[<Test>]
let ``beta reduction of a variable produces nothing`` () =
    betaReduce (v "x") |> should equal None

/// <summary>
/// A non-redex application produces nothing.
/// </summary>
[<Test>]
let ``beta reduction of a non-redex application produces nothing`` () =
    betaReduce (app (v "f") (v "x")) |> should equal None

/// <summary>
/// Beta reduction descends into the body of an abstraction.
/// </summary>
[<Test>]
let ``beta reduction descends into an abstraction body`` () =
    betaReduce (lam "x" (app (lam "y" (v "y")) (v "z")))
    |> should equal (Some (lam "x" (v "z")))

/// <summary>
/// Reduction to normal form evaluates the identity applied to itself.
/// </summary>
[<Test>]
let ``reduction to normal form of identity applied to itself`` () =
    reduceToNormalForm 100 (app (lam "x" (v "x")) (lam "x" (v "x")))
    |> should equal (lam "x" (v "x"))

/// <summary>
/// Reduction to normal form evaluates omega applied to identity.
/// </summary>
[<Test>]
let ``reduction to normal form of omega applied to identity`` () =
    let omega = lam "x" (app (v "x") (v "x"))
    reduceToNormalForm 100 (app omega (lam "x" (v "x")))
    |> should equal (lam "x" (v "x"))

/// <summary>
/// Alpha normalization makes alpha-equivalent binders textually identical.
/// </summary>
[<Test>]
let ``alpha normalization renames nested binders canonically`` () =
    normalizeAlpha (lam "y" (lam "y" (v "y")))
    |> should equal (lam "x" (lam "x1" (v "x1")))

/// <summary>
/// Alpha normalization keeps names distinct from the free variables.
/// </summary>
[<Test>]
let ``alpha normalization avoids the free variables`` () =
    normalizeAlpha (lam "a" (app (v "a") (v "x")))
    |> should equal (lam "x1" (app (v "x1") (v "x")))

/// <summary>
/// Evaluation of the K combinator selects the first argument.
/// </summary>
[<Test>]
let ``evaluation of the K combinator selects the first argument`` () =
    let k = lam "x" (lam "y" (v "x"))
    evaluate 100 Map.empty (app (app k (v "a")) (v "b"))
    |> should equal (v "a")

/// <summary>
/// Evaluation of S K K with the combinatorial definitions yields the identity.
/// </summary>
[<Test>]
let ``evaluation of S K K yields the identity`` () =
    let s =
        lam "x" (lam "y" (lam "z" (app (app (v "x") (v "z")) (app (v "y") (v "z")))))
    let k = lam "x" (lam "y" (v "x"))
    let definitions = Map.ofList [ "S", s; "K", k ]
    let expression = app (app (v "S") (v "K")) (v "K")
    evaluate 1000 definitions expression
    |> should equal (lam "x" (v "x"))

/// <summary>
/// A definition that references another definition is resolved on expansion.
/// </summary>
[<Test>]
let ``evaluation expands a chain of definitions`` () =
    let definitions = Map.ofList [ "A", lam "x" (v "x"); "B", v "A" ]
    evaluate 100 definitions (app (v "B") (v "y"))
    |> should equal (v "y")

/// <summary>
/// Definitions do not leak into the scope of a same-named bound variable.
/// </summary>
[<Test>]
let ``evaluation respects shadowing of a defined name`` () =
    let definitions = Map.ofList [ "K", lam "x" (v "x") ]
    let expression = app (lam "K" (v "K")) (v "z")
    evaluate 100 definitions expression
    |> should equal (v "z")
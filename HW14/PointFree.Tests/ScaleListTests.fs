// <copyright file="ScaleListTests.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

module HW14.Tests.ScaleListTests

open FsUnit
open FsCheck
open NUnit.Framework
open HW14.ScaleList

/// <summary>
/// Tests that original func correctly maps each element by multiplication.
/// </summary>
[<Test>]
let ``Original func returns same as manual mapping`` () =
    let x = 5
    let l = [1; 2; 3; 4; 5]
    func x l |> should equal [5; 10; 15; 20; 25]

/// <summary>
/// Tests that point-free version produces same result as original.
/// </summary>
[<Test>]
let ``PointFree func returns same as original`` () =
    let x = 3
    let l = [1; 2; 3]
    funcPointFree x l |> should equal (func x l)

/// <summary>
/// Tests that pipeline version produces same result as original.
/// </summary>
[<Test>]
let ``Pipeline func returns same as original`` () =
    let x = 4
    let l = [1; 2; 3; 4]
    funcPipeline x l |> should equal (func x l)

/// <summary>
/// Tests that all three versions produce identical results.
/// </summary>
[<Test>]
let ``All versions produce same result for various inputs`` () =
    let x = 2
    let l = [0; 1; -1; 5; -5]
    func x l |> should equal (funcPointFree x l)
    funcPointFree x l |> should equal (funcPipeline x l)

/// <summary>
/// Tests that empty list is handled correctly.
/// </summary>
[<Test>]
let ``Empty list returns empty list`` () =
    let x = 10
    func x [] |> List.isEmpty |> should be True
    funcPointFree x [] |> List.isEmpty |> should be True
    funcPipeline x [] |> List.isEmpty |> should be True

/// <summary>
/// Tests scaling by zero gives all zeros.
/// </summary>
[<Test>]
let ``Scaling by zero gives zeros`` () =
    let l = [1; 2; 3; 4; 5]
    func 0 l |> should equal [0; 0; 0; 0; 0]
    funcPointFree 0 l |> should equal [0; 0; 0; 0; 0]
    funcPipeline 0 l |> should equal [0; 0; 0; 0; 0]

/// <summary>
/// Tests that scaling by one returns the original list.
/// </summary>
[<Test>]
let ``Scaling by one returns original list`` () =
    let l = [1; 2; 3]
    func 1 l |> should equal l
    funcPointFree 1 l |> should equal l
    funcPipeline 1 l |> should equal l

/// <summary>
/// Tests negative multiplier works correctly.
/// </summary>
[<Test>]
let ``Negative multiplier works correctly`` () =
    let x = -2
    let l = [1; 2; 3]
    func x l |> should equal [-2; -4; -6]
    funcPointFree x l |> should equal [-2; -4; -6]
    funcPipeline x l |> should equal [-2; -4; -6]

/// <summary>
/// FsCheck test: verifies point-free version matches original for random inputs.
/// </summary>
[<Test>]
let ``FsCheck: funcPointFree produces same results as original for random inputs`` () =
    let prop ((x, l): (int * int list)) =
        funcPointFree x l = func x l
    prop |> Check.Quick

/// <summary>
/// FsCheck test: verifies pipeline version matches original for random inputs.
/// </summary>
[<Test>]
let ``FsCheck: funcPipeline produces same results as original for random inputs`` () =
    let prop ((x, l): (int * int list)) =
        funcPipeline x l = func x l
    prop |> Check.Quick

/// <summary>
/// FsCheck test: verifies all three functions are equivalent for random inputs.
/// </summary>
[<Test>]
let ``FsCheck: all three functions are equivalent for random inputs`` () =
    let prop ((x, l): (int * int list)) =
        func x l = funcPointFree x l && funcPointFree x l = funcPipeline x l
    prop |> Check.Quick
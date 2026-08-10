// <copyright file="ScaleListTests.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

module HW14.Tests.ScaleListTests

open FsUnit
open FsCheck
open NUnit.Framework
open HW14.ScaleList

/// <summary>
/// Tests that original scale correctly maps each element by multiplication.
/// </summary>
[<Test>]
let ``Original scale returns same as manual mapping`` () =
    let x = 5
    let l = [1; 2; 3; 4; 5]
    scale x l |> should equal [5; 10; 15; 20; 25]

/// <summary>
/// Tests that point-free version produces same result as original.
/// </summary>
[<Test>]
let ``PointFree scale returns same as original`` () =
    let x = 3
    let l = [1; 2; 3]
    scalePointFree x l |> should equal (scale x l)

/// <summary>
/// Tests that pipeline version produces same result as original.
/// </summary>
[<Test>]
let ``Pipeline scale returns same as original`` () =
    let x = 4
    let l = [1; 2; 3; 4]
    scalePipeline x l |> should equal (scale x l)

/// <summary>
/// Tests that all three versions produce identical results.
/// </summary>
[<Test>]
let ``All versions produce same result for various inputs`` () =
    let x = 2
    let l = [0; 1; -1; 5; -5]
    scale x l |> should equal (scalePointFree x l)
    scalePointFree x l |> should equal (scalePipeline x l)

/// <summary>
/// Tests that empty list is handled correctly.
/// </summary>
[<Test>]
let ``Empty list returns empty list`` () =
    let x = 10
    scale x [] |> List.isEmpty |> should be True
    scalePointFree x [] |> List.isEmpty |> should be True
    scalePipeline x [] |> List.isEmpty |> should be True

/// <summary>
/// Tests scaling by zero gives all zeros.
/// </summary>
[<Test>]
let ``Scaling by zero gives zeros`` () =
    let l = [1; 2; 3; 4; 5]
    scale 0 l |> should equal [0; 0; 0; 0; 0]
    scalePointFree 0 l |> should equal [0; 0; 0; 0; 0]
    scalePipeline 0 l |> should equal [0; 0; 0; 0; 0]

/// <summary>
/// Tests that scaling by one returns the original list.
/// </summary>
[<Test>]
let ``Scaling by one returns original list`` () =
    let l = [1; 2; 3]
    scale 1 l |> should equal l
    scalePointFree 1 l |> should equal l
    scalePipeline 1 l |> should equal l

/// <summary>
/// Tests negative multiplier works correctly.
/// </summary>
[<Test>]
let ``Negative multiplier works correctly`` () =
    let x = -2
    let l = [1; 2; 3]
    scale x l |> should equal [-2; -4; -6]
    scalePointFree x l |> should equal [-2; -4; -6]
    scalePipeline x l |> should equal [-2; -4; -6]

/// <summary>
/// FsCheck test: verifies point-free version matches original for random inputs.
/// </summary>
[<Test>]
let ``FsCheck: scalePointFree produces same results as original for random inputs`` () =
    let prop ((x, l): (int * int list)) =
        scalePointFree x l = scale x l
    prop |> Check.Quick

/// <summary>
/// FsCheck test: verifies pipeline version matches original for random inputs.
/// </summary>
[<Test>]
let ``FsCheck: scalePipeline produces same results as original for random inputs`` () =
    let prop ((x, l): (int * int list)) =
        scalePipeline x l = scale x l
    prop |> Check.Quick

/// <summary>
/// FsCheck test: verifies all three functions are equivalent for random inputs.
/// </summary>
[<Test>]
let ``FsCheck: all three functions are equivalent for random inputs`` () =
    let prop ((x, l): (int * int list)) =
        scale x l = scalePointFree x l && scalePointFree x l = scalePipeline x l
    prop |> Check.Quick
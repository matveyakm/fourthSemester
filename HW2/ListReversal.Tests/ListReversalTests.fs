// <copyright file="ListReversalTests.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

module ListReversalTests

open NUnit.Framework
open FsUnit
open ListReversal

/// <summary>
/// Tests for the list reversal function.
/// </summary>

/// <summary>
/// Tests reversing an empty list.
/// </summary>
[<Test>]
let ``reverse of empty list should return empty list`` () =
    reverse [] |> should equal []

/// <summary>
/// Tests reversing a single-element list returns the same element.
/// </summary>
[<Test>]
let ``reverse of single element list should return same list`` () =
    reverse [1] |> should equal [1]

/// <summary>
/// Tests reversing a two-element list swaps the elements.
/// </summary>
[<Test>]
let ``reverse of two element list should swap them`` () =
    reverse [1; 2] |> should equal [2; 1]

/// <summary>
/// Tests reversing a list with multiple elements.
/// </summary>
[<Test>]
let ``reverse of multiple elements should reverse order`` () =
    reverse [1; 2; 3; 4; 5] |> should equal [5; 4; 3; 2; 1]

/// <summary>
/// Tests reversing works with string lists.
/// </summary>
[<Test>]
let ``reverse of string list should work correctly`` () =
    reverse ["a"; "b"; "c"] |> should equal ["c"; "b"; "a"]

/// <summary>
/// Tests that reversing twice returns the original list.
/// </summary>
[<Test>]
let ``double reverse should return original list`` () =
    let original = [1; 2; 3; 4; 5]
    reverse (reverse original) |> should equal original
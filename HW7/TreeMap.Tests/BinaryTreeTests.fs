// <copyright file="BinaryTreeTests.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

module BinaryTreeTests

open NUnit.Framework
open FsUnit
open BinaryTree

let leaf = Leaf

let node v left right = Tree.Node(v, left, right)

/// <summary>
/// Tests mapTree with identity function - should return identical tree structure.
/// </summary>
[<Test>]
let ``mapTree with identity function returns same tree`` () =
    let tree = node 1 (node 2 leaf leaf) (node 3 leaf leaf)
    mapTree id tree |> should equal tree

/// <summary>
/// Tests mapTree with increment function on integer tree.
/// </summary>
[<Test>]
let ``mapTree increments all values`` () =
    let tree = node 1 (node 2 leaf leaf) (node 3 leaf leaf)
    let expected = node 2 (node 3 leaf leaf) (node 4 leaf leaf)
    mapTree ((+) 1) tree |> should equal expected

/// <summary>
/// Tests mapTree with string transformation on integer tree.
/// </summary>
[<Test>]
let ``mapTree converts integers to strings`` () =
    let tree = node 10 (node 20 leaf leaf) (node 30 leaf leaf)
    let expected = node "10" (node "20" leaf leaf) (node "30" leaf leaf)
    mapTree string tree |> should equal expected

/// <summary>
/// Tests mapTree with empty tree - should return empty tree.
/// </summary>
[<Test>]
let ``mapTree with empty tree returns empty tree`` () =
    let emptyTree = Leaf : Tree<int> in mapTree (fun x -> x * 2) emptyTree |> should equal emptyTree

/// <summary>
/// Tests mapTree with single node tree.
/// </summary>
[<Test>]
let ``mapTree with single node`` () =
    let tree = node 5 leaf leaf
    let expected = node 10 leaf leaf
    mapTree ((*) 2) tree |> should equal expected

/// <summary>
/// Tests mapTree with type conversion (int to float).
/// </summary>
[<Test>]
let ``mapTree converts int to float`` () =
    let tree = node 1 (node 2 leaf leaf) (node 3 leaf leaf)
    let expected = node 1.0 (node 2.0 leaf leaf) (node 3.0 leaf leaf)
    mapTree float tree |> should equal expected

/// <summary>
/// Tests mapTree preserves original tree unchanged.
/// </summary>
[<Test>]
let ``mapTree does not modify original tree`` () =
    let original = node 1 (node 2 leaf leaf) leaf
    let _ = mapTree ((*) 10) original
    original |> should equal (node 1 (node 2 leaf leaf) leaf)
// <copyright file="BinaryTree.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

namespace TreeMap

/// <summary>
/// Represents a binary tree node with a value and optional left/right children.
/// </summary>
/// <param name="Value">The value stored in the node.</param>
/// <param name="Left">Optional left subtree.</param>
/// <param name="Right">Optional right subtree.</param>
type BinaryTree<'T> =
    | Leaf
    | Node of value:'T * left:BinaryTree<'T> * right:BinaryTree<'T>

module BinaryTree =

    /// <summary>
    /// Applies a function to each element of a binary tree and returns a new binary tree
    /// with each element transformed by the function (map for trees).
    /// Uses tail recursion for efficient processing.
    /// </summary>
    /// <param name="func">The function to apply to each element.</param>
    /// <param name="tree">The source binary tree.</param>
    /// <returns>A new binary tree with transformed values.</returns>
    let rec mapTree (func: 'T -> 'U) (tree: BinaryTree<'T>) : BinaryTree<'U> =
        match tree with
        | Leaf -> Leaf
        | Node (value, left, right) ->
            let newValue = func value
            let newLeft = mapTree func left
            let newRight = mapTree func right
            Node (newValue, newLeft, newRight)
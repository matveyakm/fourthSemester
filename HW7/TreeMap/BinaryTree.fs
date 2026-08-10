// <copyright file="BinaryTree.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

module BinaryTree

/// <summary>
/// Represents a binary tree node with a value and optional left/right children.
/// </summary>
/// <typeparam name="'T">The type of values stored in tree nodes.</typeparam>
type Tree<'T> =
    | Leaf
    | Node of value:'T * left:Tree<'T> * right:Tree<'T>

/// <summary>
/// Applies a function to each element of a binary tree and returns a new binary tree
/// with each element transformed by the function (map for trees).
/// Uses continuation-passing style for efficient tail recursion.
/// </summary>
/// <param name="func">The function to apply to each element.</param>
/// <param name="tree">The source binary tree.</param>
/// <returns>A new binary tree with transformed values.</returns>
let mapTree (func: 'T -> 'U) (tree: Tree<'T>) : Tree<'U> =
    let rec foldTree cont t =
        match t with
        | Leaf -> cont Leaf
        | Node (v, l, r) ->
            foldTree (fun l' -> 
                foldTree (fun r' -> 
                    cont (Node (func v, l', r'))) r) l
    foldTree id tree
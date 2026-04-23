// <copyright file="ListReversal.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

module ListReversal

/// <summary>
/// Reverses a list in linear time using tail recursion with an accumulator.
/// </summary>
/// <param name="list">The input list to reverse.</param>
/// <returns>The reversed list.</returns>
let reverse (list: 'a list) =
    let rec reverseAcc acc = function
        | [] -> acc
        | head :: tail -> reverseAcc (head :: acc) tail
    reverseAcc [] list
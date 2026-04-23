// <copyright file="Factorial.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

module Factorial

/// <summary>
/// Calculates factorial of a non-negative integer using tail recursion.
/// Returns None for negative integers (factorial is not defined).
/// </summary>
/// <param name="n">Non-negative integer to calculate factorial for</param>
/// <returns>Factorial value wrapped in Some, or None for negative input</returns>
let factorial (n: int) : int64 option =
    if n < 0 then
        None
    else
        let rec loop (acc: int64) (counter: int) =
            if counter = 0 then
                acc
            else
                loop (acc * int64 counter) (counter - 1)
        Some(loop 1L n)
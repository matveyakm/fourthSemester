// <copyright file="FibonacciNumber.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

module Fibonacci

open System.Numerics

/// <summary>
/// Computes the n-th Fibonacci number using tail recursion for linear time complexity.
/// </summary>
/// <param name="n">The index of the Fibonacci number to compute (0-indexed).</param>
/// <returns>The n-th Fibonacci number.</returns>
let fib (n: int) =
    if n < 0 then
        invalidArg "n" "Index must be non-negative"
    
    let rec loop a b count =
        if count = 0 then a
        else loop b (a + b) (count - 1)
    
    loop 0I 1I n
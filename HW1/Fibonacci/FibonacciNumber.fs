// <copyright file="FibonacciNumber.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

module Fibonacci

open System.Numerics

/// <summary>
/// Computes the n-th Fibonacci number using tail recursion for linear time complexity.
/// </summary>
/// <param name="n">The index of the Fibonacci number to compute (0-indexed).</param>
/// <returns>Some n-th Fibonacci number, or None if n is negative.</returns>
let fib (n: int) =
    if n < 0 then None
    else
        let rec loop prev cur count =
            if count = 0 then Some prev
            else loop cur (prev + cur) (count - 1)
        
        loop 0I 1I n
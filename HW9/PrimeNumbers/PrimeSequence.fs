// <copyright file="PrimeSequence.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

module PrimeNumbers

/// <summary>
/// Checks if a number is prime using trial division.
/// </summary>
/// <param name="n">Number to check for primality</param>
let isPrime n =
    if n <= 1 then false
    elif n = 2 then true
    elif n % 2 = 0 then false
    else
        let limit = int (sqrt (float n))
        { 3 .. 2 .. limit } |> Seq.forall (fun divisor -> n % divisor <> 0)

/// <summary>
/// Generates an infinite sequence of prime numbers using lazy evaluation.
/// Uses Seq.initInfinite combined with Seq.filter to generate primes on demand.
/// </summary>
let primeSequence =
    Seq.initInfinite (fun index -> index + 2)
    |> Seq.filter isPrime
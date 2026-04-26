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
        let rec check i =
            i > limit || (n % i <> 0 && check (i + 2))
        check 3

/// <summary>
/// Generates an infinite sequence of prime numbers using lazy evaluation.
/// Uses Seq.unfold to generate primes on demand.
/// </summary>
let primeSequence =
    Seq.unfold (fun n ->
        let rec findNext candidate =
            if isPrime candidate then Some(candidate, candidate + 1)
            else findNext (candidate + 1)
        findNext n) 2
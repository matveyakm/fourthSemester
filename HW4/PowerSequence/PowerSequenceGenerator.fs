// <copyright file="PowerSequenceGenerator.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

/// <summary>
/// Generates a sequence of powers of 2 from 2^n to 2^(n + m).
/// Uses incremental multiplication to minimize exponentiations.
/// </summary>
module PowerSequence.PowerSequenceGenerator

/// <summary>
/// Generates a list [2^n; 2^(n+1); ...; 2^(n+m)] using tail-recursive approach.
/// Uses multiply-by-2 instead of repeated exponentiation for efficiency.
/// Handles negative exponents via floating-point division.
/// </summary>
/// <param name="n">Starting exponent.</param>
/// <param name="m">Number of elements to generate.</param>
/// <returns>List of powers of 2.</returns>
let generatePowerSequence n m = 
    if m < 0 then
        failwith "Number of elements to generate must be a non-negative integer."
    else
        let startValue = if n >= 0 then 2.0 ** (float n) else 1.0 / (2.0 ** (float -n))
        let rec buildSequence acc count =
            if count > m then acc
            else
                let current = if count = 0 then startValue else List.head acc * 2.0
                buildSequence (current :: acc) (count + 1)
        buildSequence [] 0 |> List.rev
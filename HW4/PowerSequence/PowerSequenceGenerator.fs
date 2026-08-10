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
/// <returns>Ok with list of powers of 2, or Error if m is negative.</returns>
let generatePowerSequence n m =
    if m < 0 then
        Error "Number of elements to generate must be a non-negative integer."
    else
        let startValue = 2.0 ** (float n)
        let rec buildSequence acc count =
            if count = 0 then acc
            else
                let current = List.head acc * 2.0
                buildSequence (current :: acc) (count - 1)
        buildSequence [startValue] m |> List.rev |> Ok
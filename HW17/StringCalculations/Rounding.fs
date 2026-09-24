// <copyright file="Rounding.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

namespace MathWorkflows

open System

/// <summary>
/// Computation expression builder for mathematical operations with a specified rounding precision.
/// </summary>
/// <param name="precision">Number of decimal places to round to at each step.</param>
type RoundingBuilder(precision: int) =
    /// <summary>
    /// Binds a value by rounding it to the specified precision before passing to the continuation.
    /// </summary>
    /// <param name="value">The float value to round.</param>
    /// <param name="f">Continuation function receiving the rounded value.</param>
    member _.Bind(value: float, f: float -> float): float =
        let rounded = Math.Round(value, precision)
        f rounded

    /// <summary>
    /// Returns a value rounded to the specified precision.
    /// </summary>
    /// <param name="value">The float value to round and return.</param>
    member _.Return(value: float): float =
        Math.Round(value, precision)

    /// <summary>
    /// Returns a value rounded to the specified precision.
    /// </summary>
    /// <param name="value">The float value to round and return.</param>
    member _.ReturnFrom(value: float): float =
        Math.Round(value, precision)

module Rounding =
    /// <summary>
    /// Creates a rounding computation expression with the given precision.
    /// </summary>
    /// <param name="precision">Number of decimal places for rounding.</param>
    /// <returns>A RoundingBuilder instance.</returns>
    let rounding precision = RoundingBuilder(precision)
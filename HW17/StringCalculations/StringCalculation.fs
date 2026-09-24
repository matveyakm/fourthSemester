// <copyright file="StringCalculation.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

namespace MathWorkflows

open System

/// <summary>
/// Computation expression builder for arithmetic operations on numbers represented as strings.
/// </summary>
/// <remarks>
/// Parses string inputs as floating-point numbers. If any input fails to parse,
/// the entire computation returns None.
/// </remarks>
type StringCalculationBuilder() =
    /// <summary>
    /// Binds a string value by attempting to parse it as a float.
    /// </summary>
    /// <param name="value">String representation of a number.</param>
    /// <param name="f">Continuation function receiving the parsed float.</param>
    /// <returns>Some(result) if parsing succeeds and continuation returns Some; otherwise None.</returns>
    member _.Bind(value: string, f: float -> float option): float option =
        match Double.TryParse(value, System.Globalization.NumberStyles.Float, System.Globalization.CultureInfo.InvariantCulture) with
        | true, v -> f v
        | _ -> None

    /// <summary>
    /// Wraps a float value in Some.
    /// </summary>
    /// <param name="value">The float value to return.</param>
    /// <returns>Some(value)</returns>
    member _.Return(value: float): float option =
        Some value

    /// <summary>
    /// Returns a float option directly.
    /// </summary>
    /// <param name="value">A float option value.</param>
    /// <returns>The same float option.</returns>
    member _.ReturnFrom(value: float option) = value

module StringCalculation =
    /// <summary>
    /// Creates a string calculation computation expression.
    /// </summary>
    /// <returns>A StringCalculationBuilder instance.</returns>
    let calculate = StringCalculationBuilder()
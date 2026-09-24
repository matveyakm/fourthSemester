// <copyright file="OperatingSystem.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

namespace Localnet

open System

/// <summary>
/// Interface for operating systems that defines infection probability.
/// </summary>
type IOperatingSystem =
    /// <summary>
    /// Gets the probability of infection for this operating system.
    /// </summary>
    abstract member InfectionProbability: float

/// <summary>
/// Windows operating system with high infection probability.
/// </summary>
type Windows() =
    interface IOperatingSystem with
        /// <summary>
        /// Windows has 85% infection probability.
        /// </summary>
        member _.InfectionProbability = 0.85

/// <summary>
/// Linux operating system with medium infection probability.
/// </summary>
type Linux() =
    interface IOperatingSystem with
        /// <summary>
        /// Linux has 30% infection probability.
        /// </summary>
        member _.InfectionProbability = 0.30

/// <summary>
/// macOS operating system with low infection probability.
/// </summary>
type MacOS() =
    interface IOperatingSystem with
        /// <summary>
        /// macOS has 15% infection probability.
        /// </summary>
        member _.InfectionProbability = 0.15
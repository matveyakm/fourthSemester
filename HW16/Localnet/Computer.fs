// <copyright file="Computer.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

namespace Localnet

open System

/// <summary>
/// Interface for random number generation to enable mocking in tests.
/// </summary>
type IRandomProvider =
    /// <summary>
    /// Generates a random double between 0.0 and 1.0.
    /// </summary>
    /// <returns>A random double value.</returns>
    abstract member NextDouble: unit -> float

/// <summary>
/// Default random provider using System.Random.
/// </summary>
type SystemRandomProvider() =
    let rng = System.Random()
    interface IRandomProvider with
        member _.NextDouble() = rng.NextDouble()

/// <summary>
/// Represents a computer in the network with an operating system and infection state.
/// </summary>
type Computer(id: int, os: IOperatingSystem, infected: bool) =
    /// <summary>
    /// Gets the unique identifier of the computer.
    /// </summary>
    member val Id = id

    /// <summary>
    /// Gets the operating system installed on the computer.
    /// </summary>
    member val OS = os

    /// <summary>
    /// Gets or sets whether the computer is infected.
    /// </summary>
    member val IsInfected = infected with get, set

    /// <summary>
    /// Creates a new computer that is initially not infected.
    /// </summary>
    /// <param name="id">The unique identifier.</param>
    /// <param name="os">The operating system.</param>
    new(id: int, os: IOperatingSystem) = Computer(id, os, false)

    /// <summary>
    /// Gets or sets the random provider for infection probability checks.
    /// </summary>
    member val RandomProvider: IRandomProvider = SystemRandomProvider() with get, set

    /// <summary>
    /// Attempts to infect the computer based on its OS infection probability.
    /// </summary>
    /// <returns>True if the computer became infected, false otherwise.</returns>
    member this.TryInfect(): bool =
        if this.RandomProvider.NextDouble() < this.OS.InfectionProbability then
            this.IsInfected <- true
            true
        else
            false

    /// <summary>
    /// Forces the computer to become infected (used for initial infection).
    /// </summary>
    member this.Infect() =
        this.IsInfected <- true
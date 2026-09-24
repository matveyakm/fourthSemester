// <copyright file="Simulation.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

namespace Localnet

/// <summary>
/// Module for running network infection simulations.
/// </summary>
module Simulation =
    /// <summary>
    /// Runs the simulation until the network state stabilizes or maxSteps is reached.
    /// </summary>
    /// <param name="network">The network to simulate.</param>
    /// <param name="maxSteps">Maximum number of steps to run.</param>
    let run (network: Network) (maxSteps: int) =
        let rec simulateStep step =
            if step >= maxSteps || not (network.CanStateChange()) then
                ()
            else
                printfn $"\n--- Step {step + 1} ---"
                network.Tick()
                network.PrintState()
                simulateStep (step + 1)

        simulateStep 0
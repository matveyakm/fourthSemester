// <copyright file="NetworkTests.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

module NetworkTests

open NUnit.Framework
open FsUnit
open Localnet

/// <summary>
/// Mock random provider that always returns a fixed probability value.
/// </summary>
type MockRandomProvider(fixedValue: float) =
    interface IRandomProvider with
        member _.NextDouble() = fixedValue

/// <summary>
/// Operating system with zero infection probability for testing.
/// </summary>
type ZeroProbOS() =
    interface IOperatingSystem with
        member _.InfectionProbability = 0.0


/// <summary>
/// Test: infection state is mutable and can be reset between test runs.
/// </summary>
[<Test>]
let ``Infection state is mutable and resettable`` () =
    let network = Network()
    let comp1 = Computer(1, Windows())
    network.AddComputer comp1

    comp1.Infect()
    comp1.IsInfected |> should be True

    comp1.IsInfected <- false
    comp1.IsInfected |> should be False


/// <summary>
/// Test: OS with zero infection probability is immune regardless of random rolls.
/// </summary>
[<Test>]
let ``Zero-probability OS is immune regardless of random rolls`` () =
    let network = Network()

    let comp1 = Computer(1, Windows())
    let comp2 = Computer(2, ZeroProbOS())

    network.AddComputer comp1
    network.AddComputer comp2

    network.Connect(1, 2)

    comp1.Infect()

    let mock = MockRandomProvider(0.0)
    comp1.RandomProvider <- mock
    comp2.RandomProvider <- mock

    network.Tick()

    comp1.IsInfected |> should be True
    comp2.IsInfected |> should be False


/// <summary>
/// Test: deterministic spread with 100% infection rate reaches all connected nodes.
/// </summary>
[<Test>]
let ``Full infection rate reaches entire connected component`` () =
    let network = Network()

    let comp1 = Computer(1, Windows())
    let comp2 = Computer(2, Linux())
    let comp3 = Computer(3, Windows())
    let comp4 = Computer(4, MacOS())
    let comp5 = Computer(5, Linux())

    network.AddComputer comp1
    network.AddComputer comp2
    network.AddComputer comp3
    network.AddComputer comp4
    network.AddComputer comp5

    network.Connect(1, 2)
    network.Connect(1, 3)
    network.Connect(2, 4)
    network.Connect(3, 5)

    comp1.Infect()

    let mock = MockRandomProvider(0.0)
    [comp1; comp2; comp3; comp4; comp5] |> List.iter (fun c -> c.RandomProvider <- mock)

    network.Tick()
    network.Tick()

    network.GetInfectedCount() |> should equal 5
    comp2.IsInfected |> should be True
    comp3.IsInfected |> should be True
    comp4.IsInfected |> should be True
    comp5.IsInfected |> should be True


/// <summary>
/// Test: CanStateChange detects remaining susceptible neighbors with non-zero OS probability.
/// </summary>
[<Test>]
let ``CanStateChange detects susceptible neighbors with non-zero OS probability`` () =
    let network = Network()

    let comp1 = Computer(1, Windows())
    let comp2 = Computer(2, Linux())

    network.AddComputer comp1
    network.AddComputer comp2

    network.Connect(1, 2)

    comp1.Infect()

    let mock = MockRandomProvider(1.0)
    comp1.RandomProvider <- mock
    comp2.RandomProvider <- mock

    network.Tick()

    network.CanStateChange() |> should be True


/// <summary>
/// Test: multi-step propagation with varying probabilities per hop.
/// </summary>
[<Test>]
let ``Multi-hop propagation respects per-node probabilities`` () =
    let network = Network()

    let comp1 = Computer(1, Windows())
    let comp2 = Computer(2, Linux())
    let comp3 = Computer(3, Windows())
    let comp4 = Computer(4, MacOS())

    network.AddComputer comp1
    network.AddComputer comp2
    network.AddComputer comp3
    network.AddComputer comp4

    network.Connect(1, 2)
    network.Connect(2, 3)
    network.Connect(3, 4)

    comp1.Infect()

    comp2.RandomProvider <- MockRandomProvider(0.0)
    comp3.RandomProvider <- MockRandomProvider(0.5)
    comp4.RandomProvider <- MockRandomProvider(1.0)

    network.Tick()
    network.Tick()

    comp2.IsInfected |> should be True
    comp4.IsInfected |> should be False


/// <summary>
/// Test: zero infection rate prevents any spread beyond patient zero.
/// </summary>
[<Test>]
let ``Zero infection rate contains virus to patient zero`` () =
    let network = Network()

    let comp1 = Computer(1, Windows())
    let comp2 = Computer(2, Linux())
    let comp3 = Computer(3, MacOS())

    network.AddComputer comp1
    network.AddComputer comp2
    network.AddComputer comp3

    network.Connect(1, 2)
    network.Connect(2, 3)

    comp1.Infect()

    let mock = MockRandomProvider(1.0)
    [comp1; comp2; comp3] |> List.iter (fun c -> c.RandomProvider <- mock)

    network.Tick()
    network.Tick()

    comp1.IsInfected |> should be True
    comp2.IsInfected |> should be False
    comp3.IsInfected |> should be False


/// <summary>
/// Test: per-node probability allows selective infection of neighbors.
/// </summary>
[<Test>]
let ``Per-node probability enables selective neighbor infection`` () =
    let network = Network()

    let comp1 = Computer(1, Windows())
    let comp2 = Computer(2, Linux())
    let comp3 = Computer(3, MacOS())

    network.AddComputer comp1
    network.AddComputer comp2
    network.AddComputer comp3

    network.Connect(1, 2)
    network.Connect(2, 3)

    comp1.Infect()

    comp2.RandomProvider <- MockRandomProvider(0.0)
    comp3.RandomProvider <- MockRandomProvider(1.0)

    network.Tick()

    comp2.IsInfected |> should be True
    comp3.IsInfected |> should be False
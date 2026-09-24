// <copyright file="Program.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

open Localnet

[<EntryPoint>]
let main _ =
    let network = Network()

    let computer1 = Computer(1, Windows())
    let computer2 = Computer(2, Linux())
    let computer3 = Computer(3, Windows())
    let computer4 = Computer(4, MacOS())
    let computer5 = Computer(5, Linux())

    network.AddComputer computer1
    network.AddComputer computer2
    network.AddComputer computer3
    network.AddComputer computer4
    network.AddComputer computer5

    network.Connect(1, 2)
    network.Connect(1, 3)
    network.Connect(2, 3)
    network.Connect(3, 4)
    network.Connect(4, 5)

    computer1.Infect()

    printfn "=== Initial State ==="
    network.PrintState()

    Simulation.run network 20

    0
// <copyright file="Network.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

namespace Localnet

open System
open System.Collections.Generic

/// <summary>
/// Represents a local network of computers connected by links.
/// </summary>
type Network() =
    let mutable computers: Map<int, Computer> = Map.empty
    let mutable adjacency: Map<int, int list> = Map.empty

    /// <summary>
    /// Adds a computer to the network.
    /// </summary>
    /// <param name="computer">The computer to add.</param>
    member this.AddComputer(computer: Computer) =
        computers <- Map.add computer.Id computer computers
        if not (Map.containsKey computer.Id adjacency) then
            adjacency <- Map.add computer.Id [] adjacency

    /// <summary>
    /// Creates a bidirectional connection between two computers.
    /// </summary>
    /// <param name="id1">The ID of the first computer.</param>
    /// <param name="id2">The ID of the second computer.</param>
    member this.Connect(id1: int, id2: int) =
        match Map.tryFind id1 computers, Map.tryFind id2 computers with
        | Some _, Some _ ->
            let addNeighbor map key neighbor =
                Map.change key (fun existing ->
                    match existing with
                    | Some neighbors -> Some (neighbor :: neighbors)
                    | None -> Some [neighbor]
                ) map

            adjacency <- addNeighbor (addNeighbor adjacency id1 id2) id2 id1
        | _ -> ()

    /// <summary>
    /// Gets a computer by its ID.
    /// </summary>
    /// <param name="id">The computer ID.</param>
    /// <returns>The computer if found, None otherwise.</returns>
    member this.GetComputer(id: int): Computer option =
        Map.tryFind id computers

    /// <summary>
    /// Gets all neighbors of a computer.
    /// </summary>
    /// <param name="id">The computer ID.</param>
    /// <returns>A list of neighbor computer IDs.</returns>
    member this.GetNeighbors(id: int): int list =
        Map.tryFind id adjacency |> Option.defaultValue []

    /// <summary>
    /// Gets all computers in the network.
    /// </summary>
    /// <returns>A sequence of all computers.</returns>
    member this.GetAllComputers(): seq<Computer> =
        computers.Values

    /// <summary>
    /// Performs one simulation step: infected computers try to infect their neighbors.
    /// </summary>
    member this.Tick(): unit =
        let currentlyInfected =
            computers.Values
            |> Seq.filter (fun c -> c.IsInfected)
            |> Seq.toList

        let candidates =
            currentlyInfected
            |> List.collect (fun c -> this.GetNeighbors c.Id)
            |> List.distinct
            |> List.choose (fun id -> this.GetComputer id)
            |> List.filter (fun c -> not c.IsInfected)

        for candidate in candidates do
            candidate.TryInfect() |> ignore

    /// <summary>
    /// Prints the current state of the network to the console.
    /// </summary>
    member this.PrintState(): unit =
        printfn "\nNetwork State:"
        for comp in computers.Values |> Seq.sortBy (fun c -> c.Id) do
            let status = if comp.IsInfected then "INFECTED" else "CLEAN"
            printfn "Computer %d [%A] - %s" comp.Id comp.OS status

    /// <summary>
    /// Gets the count of infected computers.
    /// </summary>
    /// <returns>The number of infected computers.</returns>
    member this.GetInfectedCount(): int =
        computers.Values |> Seq.filter (fun c -> c.IsInfected) |> Seq.length

    /// <summary>
    /// Checks if the network state can still change (there are uninfected neighbors of infected computers with non-zero infection probability).
    /// </summary>
    /// <returns>True if the state can change, false otherwise.</returns>
    member this.CanStateChange(): bool =
        computers.Values
        |> Seq.exists (fun c ->
            if c.IsInfected then
                this.GetNeighbors c.Id
                |> List.exists (fun neighborId ->
                    this.GetComputer neighborId
                    |> Option.exists (fun neighbor ->
                        not neighbor.IsInfected && neighbor.OS.InfectionProbability > 0.0
                    )
                )
            else
                false
        )
// <copyright file="LazyLockFree.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

namespace Lazy

open System.Threading

/// <summary>
/// A box that holds an already computed value. Using a box (instead of the raw value)
/// makes the comparison performed by the lock-free algorithm unambiguous even when the
/// computed value itself is null.
/// </summary>
/// <typeparam name="'a">The type of the held value.</typeparam>
[<AllowNullLiteral>]
type private ValueHolder<'a>(value: 'a) =
    member _.Value = value

/// <summary>
/// A lock-free ILazy implementation. Under concurrency the
/// supplier may be executed several times, but only one result is ever published and
/// every Get call returns exactly that same instance; the losing results are discarded.
/// </summary>
/// <typeparam name="'a">The type of the value produced by the lazy computation.</typeparam>
/// <param name="supplier">The function invoked to produce the value.</param>
type LazyLockFree<'a>(supplier: unit -> 'a) =
    [<VolatileField>]
    let mutable holder: ValueHolder<'a> = null

    interface ILazy<'a> with
        member _.Get() =
            match holder with
            | null ->
                let candidate = ValueHolder<'a>(supplier())
                match Interlocked.CompareExchange(&holder, candidate, null) with
                | null -> candidate.Value
                | published -> published.Value
            | published -> published.Value
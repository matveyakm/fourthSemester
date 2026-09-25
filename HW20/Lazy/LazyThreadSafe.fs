// <copyright file="LazyThreadSafe.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

namespace Lazy

/// <summary>
/// A thread-safe ILazy implementation based on double-checked
/// locking. The supplier is executed at most once even when Get is invoked from many
/// threads concurrently; the lock is only taken when the value has not been computed yet.
/// </summary>
/// <typeparam name="'a">The type of the value produced by the lazy computation.</typeparam>
/// <param name="supplier">The function invoked once to produce the value.</param>
type LazyThreadSafe<'a>(supplier: unit -> 'a) =
    [<VolatileField>]
    let mutable cachedValue: 'a option = None

    let gate = obj()

    interface ILazy<'a> with
        member _.Get() =
            match cachedValue with
            | Some cached -> cached
            | None ->
                lock gate (fun () ->
                    match cachedValue with
                    | Some cached -> cached
                    | None ->
                        let value = supplier()
                        cachedValue <- Some value
                        value)
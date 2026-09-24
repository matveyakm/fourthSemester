// <copyright file="LazySimple.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

namespace Lazy

/// <summary>
/// A straightforward ILazy implementation that is only
/// guaranteed to be correct in a single-threaded scenario. It performs no
/// synchronization at all, so concurrent Get calls may run the supplier more than once.
/// </summary>
/// <typeparam name="'a">The type of the value produced by the lazy computation.</typeparam>
/// <param name="supplier">The function invoked once to produce the value.</param>
type LazySimple<'a>(supplier: unit -> 'a) =
    let mutable cachedValue: 'a option = None

    interface ILazy<'a> with
        member _.Get() =
            match cachedValue with
            | Some cached -> cached
            | None ->
                let value = supplier()
                cachedValue <- Some value
                value
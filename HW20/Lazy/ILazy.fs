// <copyright file="ILazy.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

namespace Lazy

/// <summary>
/// Represents a lazy computation that produces a value on demand.
/// The first call to Get runs the computation, subsequent calls return the cached value.
/// </summary>
/// <typeparam name="'a">The type of the value produced by the lazy computation.</typeparam>
type ILazy<'a> =

    /// <summary>
    /// Forces the computation and returns its result.
    /// </summary>
    /// <returns>The computed value.</returns>
    abstract member Get: unit -> 'a
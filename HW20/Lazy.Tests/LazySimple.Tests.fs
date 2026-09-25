// <copyright file="LazySimple.Tests.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

namespace LazyTests

open NUnit.Framework
open Lazy
open LazyTests.Shared

/// <summary>
/// Tests for LazySimple. This implementation performs no synchronization, therefore it is
/// only required to behave correctly in a single-threaded scenario and has no concurrency
/// tests here.
/// </summary>
module LazySimpleTests =

    /// <summary>Wraps LazySimple into the ILazy interface.</summary>
    let private createLazy supplier = LazySimple(supplier) :> ILazy<_>

    /// <summary>
    /// The supplier must not run before the first Get and must be invoked exactly once.
    /// </summary>
    [<Test>]
    let ``LazySimple is lazy and caches the computed value`` () =
        checkIsLazyAndCachesValue createLazy

    /// <summary>Repeated Get calls must return the exact same object instance.</summary>
    [<Test>]
    let ``LazySimple returns the same instance on every Get`` () =
        checkReturnsSameInstance createLazy

    /// <summary>A supplier that returns null must be handled correctly.</summary>
    [<Test>]
    let ``LazySimple supports a null computed value`` () =
        checkSupportsNullResult createLazy

    /// <summary>An exception thrown by the supplier must not be cached.</summary>
    [<Test>]
    let ``LazySimple does not cache a supplier exception`` () =
        checkSupplierExceptionIsRetried createLazy
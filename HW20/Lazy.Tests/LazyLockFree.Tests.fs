// <copyright file="LazyLockFree.Tests.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

namespace LazyTests

open NUnit.Framework
open Lazy
open LazyTests.Shared

/// <summary>
/// Tests for LazyLockFree. Under concurrency the supplier may run several times,
/// however only one result is published and every Get must return that same instance.
/// </summary>
module LazyLockFreeTests =

    /// <summary>Wraps LazyLockFree into the ILazy interface.</summary>
    let private createLazy supplier = LazyLockFree(supplier) :> ILazy<_>

    /// <summary>
    /// The supplier must not run before the first Get and must be invoked exactly once.
    /// </summary>
    [<Test>]
    let ``LazyLockFree is lazy and caches the computed value`` () =
        checkIsLazyAndCachesValue createLazy

    /// <summary>Repeated Get calls must return the exact same object instance.</summary>
    [<Test>]
    let ``LazyLockFree returns the same instance on every Get`` () =
        checkReturnsSameInstance createLazy

    /// <summary>A supplier that returns null must be handled correctly.</summary>
    [<Test>]
    let ``LazyLockFree supports a null computed value`` () =
        checkSupportsNullResult createLazy

    /// <summary>An exception thrown by the supplier must not be cached.</summary>
    [<Test>]
    let ``LazyLockFree does not cache a supplier exception`` () =
        checkSupplierExceptionIsRetried createLazy

    /// <summary>
    /// Under heavy concurrency every thread must observe the same instance even though the
    /// supplier may be invoked more than once.
    /// </summary>
    [<Test>]
    let ``LazyLockFree always publishes the same instance under concurrency`` () =
        checkAlwaysPublishesSameInstance createLazy
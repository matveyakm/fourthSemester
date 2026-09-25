// <copyright file="LazyThreadSafe.Tests.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

namespace LazyTests

open NUnit.Framework
open Lazy
open LazyTests.Shared

/// <summary>
/// Tests for LazyThreadSafe, which guarantees that the supplier runs at most once
/// even when Get is invoked concurrently from many threads.
/// </summary>
module LazyThreadSafeTests =

    /// <summary>Wraps LazyThreadSafe into the ILazy interface.</summary>
    let private createLazy supplier = LazyThreadSafe(supplier) :> ILazy<_>

    /// <summary>
    /// The supplier must not run before the first Get and must be invoked exactly once.
    /// </summary>
    [<Test>]
    let ``LazyThreadSafe is lazy and caches the computed value`` () =
        checkIsLazyAndCachesValue createLazy

    /// <summary>Repeated Get calls must return the exact same object instance.</summary>
    [<Test>]
    let ``LazyThreadSafe returns the same instance on every Get`` () =
        checkReturnsSameInstance createLazy

    /// <summary>A supplier that returns null must be handled correctly.</summary>
    [<Test>]
    let ``LazyThreadSafe supports a null computed value`` () =
        checkSupportsNullResult createLazy

    /// <summary>An exception thrown by the supplier must not be cached.</summary>
    [<Test>]
    let ``LazyThreadSafe does not cache a supplier exception`` () =
        checkSupplierExceptionIsRetried createLazy

    /// <summary>
    /// Under heavy concurrency the supplier must still be executed exactly once and every
    /// thread must observe the same instance.
    /// </summary>
    [<Test>]
    let ``LazyThreadSafe computes the value exactly once under concurrency`` () =
        checkComputesExactlyOnceUnderConcurrency createLazy
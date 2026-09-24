// <copyright file="LazyTestsShared.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

namespace LazyTests

open System
open System.Threading
open NUnit.Framework
open FsUnit
open Lazy

/// <summary>
/// A fresh, featureless object returned by test suppliers so that the tests can check
/// that every Get call produces the very same object instance (reference identity).
/// </summary>
type ComputationResult() =
    class end

/// <summary>
/// Test helpers shared by all three implementation test modules. Keeping the checks
/// here lets every implementation reuse the same test logic without copy-paste.
/// </summary>
module Shared =

    /// <summary>
    /// Runs the given action on <paramref name="workerCount"/> dedicated threads that all
    /// start together after a barrier rendezvous, which maximizes the chance of a real race.
    /// Dedicated threads are used instead of thread-pool tasks so that every worker is
    /// guaranteed to be scheduled promptly.
    /// </summary>
    /// <param name="workerCount">The number of concurrent workers.</param>
    /// <param name="action">The action performed by every worker.</param>
    /// <returns>The results produced by all workers.</returns>
    let invokeFromManyThreads (workerCount: int) (action: unit -> 'a) : 'a list =
        use barrier = new Barrier(workerCount)
        let results = Array.zeroCreate workerCount
        let threads =
            [| for index in 0 .. workerCount - 1 ->
                   let thread = Thread(ThreadStart(fun () ->
                       barrier.SignalAndWait()
                       results[index] <- action()))
                   thread.IsBackground <- true
                   thread |]
        for thread in threads do
            thread.Start()
        for thread in threads do
            thread.Join()
        Array.toList results

    /// <summary>
    /// Verifies that the value is not computed before the first Get call, that the
    /// supplier then runs exactly once, and that all following Get calls return it.
    /// </summary>
    /// <param name="createLazy">Factory building a concrete Lazy implementation.</param>
    let checkIsLazyAndCachesValue (createLazy: (unit -> int) -> ILazy<int>) =
        let computationCount = ref 0
        let supplier () =
            Interlocked.Increment(computationCount) |> ignore
            19 + 48

        let lazyValue = createLazy supplier
        computationCount.Value |> should equal 0
        lazyValue.Get() |> should equal 67
        computationCount.Value |> should equal 1
        lazyValue.Get() |> should equal 67
        lazyValue.Get() |> should equal 67
        computationCount.Value |> should equal 1

    /// <summary>
    /// Verifies that repeated Get calls return the exact same object instance.
    /// </summary>
    /// <param name="createLazy">Factory building a concrete Lazy implementation.</param>
    let checkReturnsSameInstance (createLazy: (unit -> ComputationResult) -> ILazy<ComputationResult>) =
        let lazyValue = createLazy (fun () -> ComputationResult())

        let firstResult = lazyValue.Get()
        for _ in 1 .. 5 do
            Assert.AreSame(firstResult, lazyValue.Get())

    /// <summary>
    /// Verifies that a supplier returning null works and the null value is cached.
    /// </summary>
    /// <param name="createLazy">Factory building a concrete Lazy implementation.</param>
    let checkSupportsNullResult (createLazy: (unit -> string) -> ILazy<string>) =
        let lazyValue = createLazy (fun () -> null)
        lazyValue.Get() |> should be Null
        lazyValue.Get() |> should be Null

    /// <summary>
    /// Verifies that an exception thrown by the supplier is not cached, so the next
    /// Get call retries the computation.
    /// </summary>
    /// <param name="createLazy">Factory building a concrete Lazy implementation.</param>
    let checkSupplierExceptionIsRetried (createLazy: (unit -> int) -> ILazy<int>) =
        let attempts = ref 0
        let supplier () =
            attempts.Value <- attempts.Value + 1
            if attempts.Value = 1 then
                raise (InvalidOperationException("The first attempt always fails."))
            74 + 26

        let lazyValue = createLazy supplier
        (fun () -> lazyValue.Get() |> ignore) |> should throw typeof<InvalidOperationException>
        lazyValue.Get() |> should equal 100
        attempts.Value |> should equal 2

    /// <summary>
    /// Verifies that under heavy concurrency the supplier is executed exactly once and
    /// every thread observes the very same instance.
    /// </summary>
    /// <param name="createLazy">Factory building a concrete Lazy implementation.</param>
    let checkComputesExactlyOnceUnderConcurrency
        (createLazy: (unit -> ComputationResult) -> ILazy<ComputationResult>)
        =
        let computationCount = ref 0
        let supplier () =
            Interlocked.Increment(computationCount) |> ignore
            ComputationResult()

        let lazyValue = createLazy supplier
        let results = invokeFromManyThreads 64 (fun () -> lazyValue.Get())
        let reference = results.Head
        for result in results do
            Assert.AreSame(reference, result)
        Assert.AreSame(reference, lazyValue.Get())
        computationCount.Value |> should equal 1

    /// <summary>
    /// Verifies that under heavy concurrency every thread observes the same instance.
    /// The supplier may be invoked more than once; the extra results are simply discarded.
    /// </summary>
    /// <param name="createLazy">Factory building a concrete Lazy implementation.</param>
    let checkAlwaysPublishesSameInstance
        (createLazy: (unit -> ComputationResult) -> ILazy<ComputationResult>)
        =
        let computationCount = ref 0
        let supplier () =
            Interlocked.Increment(computationCount) |> ignore
            ComputationResult()

        let lazyValue = createLazy supplier
        let results = invokeFromManyThreads 64 (fun () -> lazyValue.Get())
        let reference = results.Head
        for result in results do
            Assert.AreSame(reference, result)
        Assert.That(computationCount.Value, Is.GreaterThanOrEqualTo(1))
        Assert.AreSame(reference, lazyValue.Get())
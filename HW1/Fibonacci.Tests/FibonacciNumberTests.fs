// <copyright file="FibonacciNumberTests.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

module FibonacciNumberTests

open System.Numerics
open NUnit.Framework
open FsUnit
open Fibonacci

/// <summary>
/// Tests for Fibonacci number computation.
/// </summary>
type public FibonacciNumberTests() =
    
    /// <summary>Fib(0) = 0</summary>
    [<Test>]
    member public this.``fib of 0 should be 0``() =
        fib 0 |> should equal 0I
    
    /// <summary>Fib(1) = 1</summary>
    [<Test>]
    member public this.``fib of 1 should be 1``() =
        fib 1 |> should equal 1I
    
    /// <summary>Fib(2) = 1</summary>
    [<Test>]
    member public this.``fib of 2 should be 1``() =
        fib 2 |> should equal 1I
    
    /// <summary>Fib(3) = 2</summary>
    [<Test>]
    member public this.``fib of 3 should be 2``() =
        fib 3 |> should equal 2I
    
    /// <summary>Fib(4) = 3</summary>
    [<Test>]
    member public this.``fib of 4 should be 3``() =
        fib 4 |> should equal 3I
    
    /// <summary>Fib(5) = 5</summary>
    [<Test>]
    member public this.``fib of 5 should be 5``() =
        fib 5 |> should equal 5I
    
    /// <summary>Fib(10) = 55</summary>
    [<Test>]
    member public this.``fib of 10 should be 55``() =
        fib 10 |> should equal 55I
    
    /// <summary>Fib(20) = 6765</summary>
    [<Test>]
    member public this.``fib of 20 should be 6765``() =
        fib 20 |> should equal 6765I
    
    /// <summary>Negative index should throw ArgumentException</summary>
    [<Test>]
    member public this.``fib of negative should throw``() =
        (fun () -> fib -1 |> ignore) |> should throw typeof<System.ArgumentException>
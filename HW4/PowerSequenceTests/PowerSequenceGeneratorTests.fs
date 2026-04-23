// <copyright file="PowerSequenceGeneratorTests.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

module PowerSequenceTests

open NUnit.Framework
open FsUnit
open PowerSequence.PowerSequenceGenerator

/// <summary>
/// Tests basic functionality of generatePowerSequence function.
/// </summary>
[<Test>]
let ``generatePowerSequence should return [2.0] for n=1, m=0`` () =
    generatePowerSequence 1 0 |> should equal [2.0]

/// <summary>
/// Tests that sequence contains correct number of elements.
/// </summary>
[<Test>]
let ``generatePowerSequence should return list of length m+1`` () =
    generatePowerSequence 0 4 |> should haveLength 5

/// <summary>
/// Tests correctness of sequence starting from 2^0 = 1.
/// </summary>
[<Test>]
let ``generatePowerSequence should return [1.0; 2.0; 4.0; 8.0] for n=0, m=3`` () =
    generatePowerSequence 0 3 |> should equal [1.0; 2.0; 4.0; 8.0]

/// <summary>
/// Tests correctness of sequence starting from 2^3 = 8.
/// </summary>
[<Test>]
let ``generatePowerSequence should return [8.0; 16.0; 32.0] for n=3, m=2`` () =
    generatePowerSequence 3 2 |> should equal [8.0; 16.0; 32.0]

/// <summary>
/// Tests with larger exponents.
/// </summary>
[<Test>]
let ``generatePowerSequence should return [1024.0; 2048.0; 4096.0; 8192.0; 16384.0] for n=10, m=4`` () =
    generatePowerSequence 10 4 |> should equal [1024.0; 2048.0; 4096.0; 8192.0; 16384.0]

/// <summary>
/// Tests with negative starting exponent (fractional values).
/// </summary>
[<Test>]
let ``generatePowerSequence should return correct values for n=-1, m=2`` () =
    generatePowerSequence -1 2 |> should equal [0.5; 1.0; 2.0]

/// <summary>
/// Tests that sequence elements are consecutive powers of 2.
/// </summary>
[<Test>]
let ``generatePowerSequence should produce consecutive powers of 2`` () =
    let result = generatePowerSequence 5 5
    let expected = [32.0; 64.0; 128.0; 256.0; 512.0; 1024.0]
    result |> should equal expected
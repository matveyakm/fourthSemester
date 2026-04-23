// <copyright file="PrimeSequenceTests.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

module PrimeSequenceTests

open NUnit.Framework

[<Test>]
let ``isPrime works correctly for first 5 numbers`` () =
    Assert.That(PrimeNumbers.isPrime 1, Is.False)
    Assert.That(PrimeNumbers.isPrime 2, Is.True)
    Assert.That(PrimeNumbers.isPrime 3, Is.True)
    Assert.That(PrimeNumbers.isPrime 4, Is.False)
    Assert.That(PrimeNumbers.isPrime 5, Is.True)

[<Test>]
let ``isPrime works correctly for numbers 6-11`` () =
    Assert.That(PrimeNumbers.isPrime 6, Is.False)
    Assert.That(PrimeNumbers.isPrime 7, Is.True)
    Assert.That(PrimeNumbers.isPrime 8, Is.False)
    Assert.That(PrimeNumbers.isPrime 9, Is.False)
    Assert.That(PrimeNumbers.isPrime 10, Is.False)
    Assert.That(PrimeNumbers.isPrime 11, Is.True)

[<Test>]
let ``primeSequence first element is 2`` () =
    Assert.That(Seq.head PrimeNumbers.primeSequence, Is.EqualTo(2))

[<Test>]
let ``primeSequence second element is 3`` () =
    Assert.That(Seq.item 1 PrimeNumbers.primeSequence, Is.EqualTo(3))

[<Test>]
let ``primeSequence third element is 5`` () =
    Assert.That(Seq.item 2 PrimeNumbers.primeSequence, Is.EqualTo(5))

[<Test>]
let ``primeSequence first 10 primes are correct`` () =
    let expected = [2; 3; 5; 7; 11; 13; 17; 19; 23; 29]
    let actual = PrimeNumbers.primeSequence |> Seq.take 10 |> Seq.toList
    Assert.That(actual.Equals(expected))

[<Test>]
let ``primeSequence first 15 primes contain expected primes`` () =
    let expected = [2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47]
    let actual = PrimeNumbers.primeSequence |> Seq.take 15 |> Seq.toList
    Assert.That(actual.Equals(expected))

[<Test>]
let ``primeSequence contains primes up to 100`` () =
    let primesUpTo100 = [2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47; 53; 59; 61; 67; 71; 73; 79; 83; 89; 97]
    let actual = PrimeNumbers.primeSequence |> Seq.take 25 |> Seq.toList
    Assert.That(actual.Equals(primesUpTo100))

[<Test>]
let ``primeSequence is infinite`` () =
    let first100 = PrimeNumbers.primeSequence |> Seq.take 100 |> Seq.length
    Assert.That(first100, Is.EqualTo(100))
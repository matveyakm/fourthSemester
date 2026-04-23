// <copyright file="ListIndexOfTests.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

module ListIndexOf.Tests

open NUnit.Framework
open ListIndexOf

[<Test>]
let ``findIndex returns Some 0 for element at first position`` () =
    let result = findIndex 1 [1; 2; 3]
    Assert.That(result, Is.EqualTo(Some 0))

[<Test>]
let ``findIndex returns Some 1 for element at second position`` () =
    let result = findIndex 2 [1; 2; 3]
    Assert.That(result, Is.EqualTo(Some 1))

[<Test>]
let ``findIndex returns Some 2 for element at third position`` () =
    let result = findIndex 3 [1; 2; 3]
    Assert.That(result, Is.EqualTo(Some 2))

[<Test>]
let ``findIndex returns None for element not in list`` () =
    let result = findIndex 4 [1; 2; 3]
    Assert.That(result, Is.EqualTo(None))

[<Test>]
let ``findIndex returns None for empty list`` () =
    let result = findIndex 1 []
    Assert.That(result, Is.EqualTo(None))

[<Test>]
let ``findIndex returns Some 0 when looking for first element and there is only one`` () =
    let result = findIndex 5 [5]
    Assert.That(result, Is.EqualTo(Some 0))

[<Test>]
let ``findIndex returns first occurrence for duplicated elements`` () =
    let result = findIndex 2 [1; 2; 2; 3]
    Assert.That(result, Is.EqualTo(Some 1))

[<Test>]
let ``findIndex with negative numbers`` () =
    let result = findIndex -5 [-10; -5; 0; 5]
    Assert.That(result, Is.EqualTo(Some 1))

[<Test>]
let ``findIndex works with strings`` () =
    let result = findIndex "b" ["a"; "b"; "c"]
    Assert.That(result, Is.EqualTo(Some 1))

[<Test>]
let ``findIndex with larger list`` () =
    let result = findIndex 5 [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
    Assert.That(result, Is.EqualTo(Some 4))
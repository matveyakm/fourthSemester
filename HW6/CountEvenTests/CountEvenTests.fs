// <copyright file="CountEvenTests.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

module CountEvenTests

open FsUnit
open FsCheck
open NUnit.Framework
open CountEven

[<Test>]
let ``countEvenWithMap returns correct count for known list`` () =
    countEvenWithMap [1; 2; 3; 4; 5; 6] |> should equal 3

[<Test>]
let ``countEvenWithFilter returns correct count for known list`` () =
    countEvenWithFilter [1; 2; 3; 4; 5; 6] |> should equal 3

[<Test>]
let ``countEvenWithFold returns correct count for known list`` () =
    countEvenWithFold [1; 2; 3; 4; 5; 6] |> should equal 3

[<Test>]
let ``countEvenWithMap returns 0 for empty list`` () =
    countEvenWithMap [] |> should equal 0

[<Test>]
let ``countEvenWithFilter returns 0 for empty list`` () =
    countEvenWithFilter [] |> should equal 0

[<Test>]
let ``countEvenWithFold returns 0 for empty list`` () =
    countEvenWithFold [] |> should equal 0

[<Test>]
let ``All three functions are equivalent for arbitrary lists`` () =
    let prop (list: int list) =
        countEvenWithMap list = countEvenWithFilter list
        && countEvenWithFilter list = countEvenWithFold list
    FsCheck.Check.Quick prop

[<Test>]
let ``countEvenWithMap and countEvenWithFilter are equivalent`` () =
    let prop (list: int list) = countEvenWithMap list = countEvenWithFilter list
    FsCheck.Check.Quick prop

[<Test>]
let ``countEvenWithFilter and countEvenWithFold are equivalent`` () =
    let prop (list: int list) = countEvenWithFilter list = countEvenWithFold list
    FsCheck.Check.Quick prop

[<Test>]
let ``countEvenWithMap and countEvenWithFold are equivalent`` () =
    let prop (list: int list) = countEvenWithMap list = countEvenWithFold list
    FsCheck.Check.Quick prop

[<Test>]
let ``All functions handle negative even numbers correctly`` () =
    countEvenWithMap [-4; -2; 0; 1; 3] |> should equal 3
    countEvenWithFilter [-4; -2; 0; 1; 3] |> should equal 3
    countEvenWithFold [-4; -2; 0; 1; 3] |> should equal 3
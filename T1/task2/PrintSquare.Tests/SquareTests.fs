module PrintSquare.Tests

open NUnit.Framework
open FsUnit
open PrintSquare.Square

[<SetUp>]
let Setup () =
    ()

[<Test>]
let ``getSquare returns empty list for n = 0`` () =
    getSquare 0 |> should be Empty

[<Test>]
let ``getSquare returns empty list for negative n`` () =
    getSquare (-5) |> should be Empty

[<Test>]
let ``getSquare returns single star for n = 1`` () =
    getSquare 1 |> should equal ["*"]

[<Test>]
let ``getSquare returns correct square for n = 2`` () =
    getSquare 2 |> should equal ["**"; "**"]

[<Test>]
let ``getSquare returns correct square for n = 3`` () =
    getSquare 3 |> should equal ["***"; "* *"; "***"]

[<Test>]
let ``getSquare returns correct square for n = 4`` () =
    getSquare 4 |> should equal ["****"; "*  *"; "*  *"; "****"]

[<Test>]
let ``getSquare returns correct square for n = 5`` () =
    getSquare 5 |> should equal ["*****"; "*   *"; "*   *"; "*   *"; "*****"]
module ListMin.Tests

open NUnit.Framework
open FsUnit
open ListMin.MinFinder

[<SetUp>]
let Setup () =
    ()

[<Test>]
let ``findMin returns smallest element in list`` () =
    findMin [5; 3; 7; 1; 9] |> should equal 1

[<Test>]
let ``findMin works with single element list`` () =
    findMin [42] |> should equal 42

[<Test>]
let ``findMin works with negative numbers`` () =
    findMin [-5; -10; -3; -1] |> should equal -10

[<Test>]
let ``findMin works with floating point numbers`` () =
    findMin [3.14; 2.71; 1.41; 1.73] |> should equal 1.41

[<Test>]
let ``findMin throws on empty list`` () =
    (fun () -> findMin [] |> ignore) |> should throw typeof<System.ArgumentException>

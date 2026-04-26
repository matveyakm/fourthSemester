// <copyright file="BracketValidatorTests.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

module BracketBalance.Tests.BracketValidatorTests

open NUnit.Framework
open FsUnit
open BracketBalance.BracketValidator

/// <summary>
/// Tests for empty and trivial inputs
/// </summary>
[<Test>]
let ``Empty string should be valid`` () =
    validateBrackets "" |> should be True

/// <summary>
/// Tests for valid simple bracket sequences
/// </summary>
[<Test>]
let ``Simple parentheses should be valid`` () =
    validateBrackets "()" |> should be True

[<Test>]
let ``Simple square brackets should be valid`` () =
    validateBrackets "[]" |> should be True

[<Test>]
let ``Simple curly braces should be valid`` () =
    validateBrackets "{}" |> should be True

/// <summary>
/// Tests for nested valid bracket sequences
/// </summary>
[<Test>]
let ``Nested parentheses should be valid`` () =
    validateBrackets "(())" |> should be True

[<Test>]
let ``Nested mixed brackets should be valid`` () =
    validateBrackets "({[]})" |> should be True

[<Test>]
let ``Deeply nested brackets should be valid`` () =
    validateBrackets "((({})))" |> should be True

[<Test>]
let ``Multiple nested groups should be valid`` () =
    validateBrackets "({[]})({[]})" |> should be True

/// <summary>
/// Tests for valid sequences with other characters
/// </summary>
[<Test>]
let ``Brackets with text should be valid`` () =
    validateBrackets "hello (world) test" |> should be True

[<Test>]
let ``Complex valid sequence with text should be valid`` () =
    validateBrackets "function foo(a: int[], b: {key: value}) -> bool" |> should be True

/// <summary>
/// Tests for invalid bracket sequences
/// </summary>
[<Test>]
let ``Unclosed parentheses should be invalid`` () =
    validateBrackets "(" |> should be False

[<Test>]
let ``Unclosed square brackets should be invalid`` () =
    validateBrackets "[" |> should be False

[<Test>]
let ``Unclosed curly braces should be invalid`` () =
    validateBrackets "{" |> should be False

[<Test>]
let ``Mismatched closing parenthesis should be invalid`` () =
    validateBrackets ")" |> should be False

[<Test>]
let ``Mismatched closing square bracket should be invalid`` () =
    validateBrackets "]" |> should be False

[<Test>]
let ``Mismatched closing curly brace should be invalid`` () =
    validateBrackets "}" |> should be False

[<Test>]
let ``Incorrectly nested brackets should be invalid`` () =
    validateBrackets "(]" |> should be False

[<Test>]
let ``Wrong closing order should be invalid`` () =
    validateBrackets "([)]" |> should be False

[<Test>]
let ``Premature closing should be invalid`` () =
    validateBrackets "({]})" |> should be False

/// <summary>
/// Tests for mixed sequences with text
/// </summary>
[<Test>]
let ``Brackets with other chars in wrong order should be invalid`` () =
    validateBrackets "text (here] more" |> should be False
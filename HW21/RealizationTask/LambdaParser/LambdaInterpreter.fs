// <copyright file="LambdaInterpreter.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

/// <summary>
/// High-level entry point of the lambda calculus interpreter. It accepts a program
/// given either as a string or read from a file, expands the named definitions,
/// performs beta-reduction and returns the resulting term as a string.
/// </summary>
module LambdaParser.Interpreter

open LambdaParser.LambdaExpression
open LambdaParser.BetaReduction
open LambdaParser.Parser

/// <summary>
/// Default bound on the number of beta-reduction steps, so that programs that
/// diverge cannot hang the interpreter forever.
/// </summary>
let defaultMaxSteps = 1000

/// <summary>
/// Evaluates a parsed program: definitions accumulate in source order, and the
/// last lambda expression of the program is reduced against all of them.
/// </summary>
/// <param name="maxSteps">Maximum number of beta-reduction steps.</param>
/// <param name="program">The parsed program to evaluate.</param>
/// <returns>Ok with the reduced expression, or Error if the program has no expression.</returns>
let evaluateProgram maxSteps (program: Program) : Result<LambdaExpression, string> =
    let rec walk definitions lastResult remaining =
        match remaining with
        | [] -> lastResult
        | item :: rest ->
            match item with
            | DefinitionItem definition ->
                let definitionsAfter =
                    Map.add definition.Name definition.Body definitions
                walk definitionsAfter lastResult rest
            | ExpressionItem expression ->
                let evaluated = evaluate maxSteps definitions expression
                walk definitions (Some evaluated) rest

    match walk Map.empty None program.Items with
    | Some result -> Ok result
    | None -> Error "a program must contain at least one lambda expression to reduce"

/// <summary>
/// Parses a program given as a string and returns the text of the reduced last
/// lambda expression. Definitions and expressions may alternate freely.
/// </summary>
/// <param name="inputText">The source text of the program.</param>
/// <returns>Ok with the result string, or Error with a diagnostic message.</returns>
let interpret (inputText: string) : Result<string, string> =
    tryParseProgram inputText
    |> Result.bind (evaluateProgram defaultMaxSteps)
    |> Result.map toText

/// <summary>
/// Reads a program from a file and returns the text of the reduced last
/// lambda expression, exactly as the string-based entry point does.
/// </summary>
/// <param name="filePath">Path to the file containing the program.</param>
/// <returns>Ok with the result string, or Error with a diagnostic message.</returns>
let interpretFile (filePath: string) : Result<string, string> =
    try
        let contents = System.IO.File.ReadAllText filePath
        interpret contents
    with
    | :? System.IO.IOException as error ->
        Error ("failed to read the file: " + error.Message)
    | :? System.UnauthorizedAccessException as error ->
        Error ("failed to read the file: " + error.Message)
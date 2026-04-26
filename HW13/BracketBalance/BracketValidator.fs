// <copyright file="BracketValidator.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

module BracketBalance.BracketValidator

/// <summary>
/// Checks if a character is an opening bracket
/// </summary>
let private isOpening = function
    | '(' | '[' | '{' -> true
    | _ -> false

/// <summary>
/// Gets the matching closing bracket for an opening bracket
/// </summary>
let private getClosingBracket = function
    | '(' -> ')'
    | '[' -> ']'
    | '{' -> '}'
    | _ -> failwith "Not an opening bracket"

/// <summary>
/// Checks if two brackets match (one opening, one closing)
/// </summary>
let private isMatching closingBracket stack =
    match stack with
    | [] -> false
    | opening::rest ->
        let expectedClosing = getClosingBracket opening
        expectedClosing = closingBracket

/// <summary>
/// Validates bracket sequence using a stack-based approach with tail recursion
/// </summary>
let validateBrackets (input: string) =
    let chars = Seq.toList input
    
    let rec validateTailRecursive (remaining: list<char>) (stack: list<char>) =
        match remaining with
        | [] -> List.isEmpty stack
        | current::rest ->
            if isOpening current then
                validateTailRecursive rest (current::stack)
            else
                match current with
                | ')' | ']' | '}' when isMatching current stack ->
                    validateTailRecursive rest (List.tail stack)
                | _ when not (isOpening current) && not (current = ')' || current = ']' || current = '}') ->
                    validateTailRecursive rest stack
                | _ -> false
    
    validateTailRecursive chars []
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
/// Gets the matching closing bracket for an opening bracket,
/// or None if the character is not an opening bracket
/// </summary>
let private getClosingBracket = function
    | '(' -> Some ')'
    | '[' -> Some ']'
    | '{' -> Some '}'
    | _ -> None

/// <summary>
/// Checks if the given closing bracket matches the opening bracket on top of the stack
/// </summary>
let private isMatching closingBracket stack =
    match stack with
    | [] -> false
    | opening::_ -> getClosingBracket opening = Some closingBracket

/// <summary>
/// Validates bracket sequence using a stack-based approach with tail recursion
/// </summary>
let validateBrackets (input: string) =
    let chars = Seq.toList input
    
    let rec validateTailRecursive (remaining: list<char>) (stack: list<char>) =
        match remaining with
        | [] -> List.isEmpty stack
        | current::rest when isOpening current ->
            validateTailRecursive rest (current::stack)
        | current::rest ->
            match current with
            | ')' | ']' | '}' when isMatching current stack ->
                validateTailRecursive rest (List.tail stack)
            | ')' | ']' | '}' -> false
            | _ -> validateTailRecursive rest stack
    
    validateTailRecursive chars []

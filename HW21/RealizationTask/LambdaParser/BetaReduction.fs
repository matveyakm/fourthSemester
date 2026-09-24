// <copyright file="BetaReduction.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

/// <summary>
/// Beta-reduction engine: free variables, capture-avoiding substitution,
/// expansion of named definitions, reduction to normal form and
/// alpha-canonical form of the result.
/// </summary>
module LambdaParser.BetaReduction

open LambdaParser.LambdaExpression

/// <summary>
/// Collects all free variables of a lambda expression.
/// </summary>
/// <param name="expression">The expression to analyse.</param>
/// <returns>The set of names that occur free in the expression.</returns>
let rec freeVariables expression : Set<string> =
    match expression with
    | Variable name -> Set.singleton name
    | Abstraction (variable, body) -> Set.remove variable (freeVariables body)
    | Application (functionExpression, argumentExpression) ->
        Set.union (freeVariables functionExpression) (freeVariables argumentExpression)

/// <summary>
/// Generates a variable name based on the given one that is not present in the used set.
/// </summary>
/// <param name="used">Names that must not be reused.</param>
/// <param name="baseName">Preferred name of the variable.</param>
/// <returns>The first unused candidate name derived from the base name.</returns>
let private generateFreshName used baseName =
    let rec tryCandidate index =
        let candidate = if index = 0 then baseName else sprintf "%s_%d" baseName index
        if Set.contains candidate used then
            tryCandidate (index + 1)
        else
            candidate
    tryCandidate 0

/// <summary>
/// Substitutes all free occurrences of a variable with a replacement expression.
/// Performs alpha-renaming of a binder when keeping its name would capture
/// a free variable of the replacement.
/// </summary>
/// <param name="expression">The expression to substitute in.</param>
/// <param name="variable">The name being replaced.</param>
/// <param name="replacement">The expression to substitute in place of the variable.</param>
/// <returns>The expression with all free occurrences of the variable replaced.</returns>
let rec substitute expression variable replacement =
    match expression with
    | Variable name when name = variable -> replacement
    | Variable _ -> expression
    | Abstraction (boundVariable, _) when boundVariable = variable -> expression
    | Abstraction (boundVariable, body) ->
        let freeInBody = freeVariables body
        let freeInReplacement = freeVariables replacement
        if Set.contains variable freeInBody && Set.contains boundVariable freeInReplacement then
            let freshBoundVariable =
                generateFreshName (Set.add boundVariable freeInReplacement) boundVariable
            let renamedBody = substitute body boundVariable (Variable freshBoundVariable)
            Abstraction (freshBoundVariable, substitute renamedBody variable replacement)
        elif Set.contains variable freeInBody then
            Abstraction (boundVariable, substitute body variable replacement)
        else
            expression
    | Application (functionExpression, argumentExpression) ->
        Application (
            substitute functionExpression variable replacement,
            substitute argumentExpression variable replacement)

/// <summary>
/// Performs a single beta-reduction step in normal order (the leftmost-outermost redex).
/// Returns None when the expression is already in normal form.
/// </summary>
/// <param name="expression">The expression to reduce.</param>
/// <returns>Some reduced expression, or None for a term in normal form.</returns>
let rec betaReduce expression =
    match expression with
    | Application (Abstraction (boundVariable, body), argument) ->
        Some (substitute body boundVariable argument)
    | Application (functionExpression, argumentExpression) ->
        match betaReduce functionExpression with
        | Some reducedFunction -> Some (Application (reducedFunction, argumentExpression))
        | None ->
            match betaReduce argumentExpression with
            | Some reducedArgument -> Some (Application (functionExpression, reducedArgument))
            | None -> None
    | Abstraction (boundVariable, body) ->
        match betaReduce body with
        | Some reducedBody -> Some (Abstraction (boundVariable, reducedBody))
        | None -> None
    | Variable _ -> None

/// <summary>
/// Applies beta-reduction until a normal form is reached or the step limit runs out.
/// The step limit keeps non-terminating programs under control.
/// </summary>
/// <param name="maxSteps">Maximum number of beta-reduction steps.</param>
/// <param name="expression">The expression to reduce.</param>
/// <returns>The resulting normal form.</returns>
let rec reduceToNormalForm maxSteps expression =
    if maxSteps <= 0 then
        expression
    else
        match betaReduce expression with
        | Some reduced -> reduceToNormalForm (maxSteps - 1) reduced
        | None -> expression

/// <summary>
/// Renames every bound variable to a canonical name "x", "x1", "x2", ... that is
/// distinct from the free variables of the expression, so that alpha-equivalent
/// normal forms become textually identical.
/// </summary>
/// <param name="expression">The expression to normalize.</param>
/// <returns>An alpha-equivalent expression with canonical bound variable names.</returns>
let normalizeAlpha expression =
    let initiallyUsed = freeVariables expression

    let rec walk renaming used expression =
        match expression with
        | Variable name ->
            match Map.tryFind name renaming with
            | Some canonicalName -> Variable canonicalName
            | None -> expression
        | Abstraction (boundVariable, body) ->
            let canonicalName =
                Seq.initInfinite (fun index -> if index = 0 then "x" else sprintf "x%d" index)
                |> Seq.skipWhile (fun candidate -> Set.contains candidate used)
                |> Seq.head
            let usedAfter = Set.add canonicalName used
            let renamingAfter = Map.add boundVariable canonicalName renaming
            Abstraction (canonicalName, walk renamingAfter usedAfter body)
        | Application (functionExpression, argumentExpression) ->
            Application (
                walk renaming used functionExpression,
                walk renaming used argumentExpression)

    walk Map.empty initiallyUsed expression

/// <summary>
/// Replaces free occurrences of defined names with their definition bodies.
/// A name that is bound by an enclosing abstraction is left untouched, and a
/// cyclic definition chain is broken by leaving such a name as is.
/// </summary>
/// <param name="definitions">The map from a name to the expression it is defined as.</param>
/// <param name="boundVariables">Names bound by abstractions enclosing the current node.</param>
/// <param name="expansionChain">Names currently being expanded, used to break cycles.</param>
/// <param name="expression">The expression to expand.</param>
/// <returns>The expression with all defined free names replaced by their bodies.</returns>
let rec expandDefinitions definitions boundVariables expansionChain expression =
    match expression with
    | Variable name
        when not (Set.contains name boundVariables)
             && not (Set.contains name expansionChain)
             && Map.containsKey name definitions ->
        expandDefinitions
            definitions
            boundVariables
            (Set.add name expansionChain)
            definitions.[name]
    | Variable _ -> expression
    | Abstraction (boundVariable, body) ->
        Abstraction (
            boundVariable,
            expandDefinitions definitions (Set.add boundVariable boundVariables) expansionChain body)
    | Application (functionExpression, argumentExpression) ->
        Application (
            expandDefinitions definitions boundVariables expansionChain functionExpression,
            expandDefinitions definitions boundVariables expansionChain argumentExpression)

/// <summary>
/// Reduces an expression to the alpha-canonical normal form using the given definitions.
/// Defined names are expanded first, then the term is beta-reduced and finally
/// its bound variables are renamed to canonical ones.
/// </summary>
/// <param name="maxSteps">Maximum number of beta-reduction steps.</param>
/// <param name="definitions">The map of named definitions available to the expression.</param>
/// <param name="expression">The expression to evaluate.</param>
/// <returns>The alpha-canonical normal form of the expression.</returns>
let evaluate maxSteps definitions expression =
    expression
    |> expandDefinitions definitions Set.empty Set.empty
    |> reduceToNormalForm maxSteps
    |> normalizeAlpha
// <copyright file="LambdaExpr.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

/// <summary>
/// Module for lambda calculus: lambda expressions, alpha-conversion and beta-reduction
/// </summary>
module LambdaCalc

    /// <summary>
    /// Lambda expression defined as discriminated union
    /// </summary>
    type LambdaExpr =
        | Var of string
        | Lam of string * LambdaExpr
        | App of LambdaExpr * LambdaExpr

    /// <summary>
    /// Generate a fresh variable name that doesn't conflict with given variables
    /// </summary>
    let private generateFreshVar (used: Set<string>) (baseName: string) =
        let rec generate n =
            let candidate = if n = 0 then baseName else sprintf "%s_%d" baseName n
            if used.Contains(candidate) then generate (n + 1)
            else candidate
        generate 0

    /// <summary>
    /// Get all free variables in a lambda expression
    /// </summary>
    let rec freeVariables (expr: LambdaExpr) : Set<string> =
        match expr with
        | Var x -> Set.singleton x
        | Lam (x, body) -> freeVariables body |> Set.remove x
        | App (e1, e2) -> Set.union (freeVariables e1) (freeVariables e2)

    /// <summary>
    /// Capture-avoiding substitution: substitute all free occurrences of x with replacement
    /// </summary>
    let rec substitute term x replacement =
        match term with
        | Var y when y = x -> replacement
        | Var _ -> term
        | Lam (y, _) when y = x -> term
        | Lam (y, bodyExpr) ->
            let freeInBody = freeVariables bodyExpr
            let freeInReplacement = freeVariables replacement
            if Set.contains x freeInBody && Set.contains y freeInReplacement then
                let newY = generateFreshVar (Set.add y freeInReplacement) y
                let newBody = substitute bodyExpr y (Var newY)
                Lam (newY, substitute newBody x replacement)
            elif Set.contains x freeInBody then
                Lam (y, substitute bodyExpr x replacement)
            else
                Lam (y, bodyExpr)
        | App (e1, e2) ->
            App (substitute e1 x replacement, substitute e2 x replacement)

    /// <summary>
    /// Single beta-redex reduction step (normal order / leftmost-outermost)
    /// </summary>
    let rec betaReduce (expr: LambdaExpr) : LambdaExpr option =
        match expr with
        | App (Lam (x, body), arg) ->
            Some (substitute body x arg)
        | App (e1, e2) ->
            match betaReduce e1 with
            | Some reducedE1 -> Some (App (reducedE1, e2))
            | None ->
                match betaReduce e2 with
                | Some reducedE2 -> Some (App (e1, reducedE2))
                | None -> None
        | Lam (x, body) ->
            match betaReduce body with
            | Some reducedBody -> Some (Lam (x, reducedBody))
            | None -> None
        | Var _ -> None

    /// <summary>
    /// Full beta-reduction to normal form (normal strategy) with a limit on the number of steps
    /// </summary>
    let rec evaluate maxSteps expr =
        if maxSteps <= 0 then expr
        else
            match betaReduce expr with
            | Some reduced -> evaluate (maxSteps - 1) reduced
            | None -> expr
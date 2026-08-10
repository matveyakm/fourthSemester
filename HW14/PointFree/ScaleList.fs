// <copyright file="ScaleList.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

module HW14.ScaleList

/// <summary>
/// Original function: takes a scalar x and a list l, returns a new list where each element is multiplied by x.
/// </summary>
/// <param name="x">The scalar multiplier</param>
/// <param name="l">The input list</param>
/// <returns>A new list with each element multiplied by x</returns>
let scale x l = List.map (fun y -> y * x) l

/// <summary>
/// Point-free version of scale: composed from List.map and the multiplication
/// operator, it preserves the same behaviour without naming any arguments.
/// </summary>
let scalePointFree = List.map << (*)

/// <summary>
/// Pipeline version using |> operator to pass the list.
/// </summary>
let scalePipeline x l = l |> List.map ((*) x)
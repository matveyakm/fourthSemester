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
let func x l = List.map (fun y -> y * x) l

/// <summary>
/// Point-free version using operator section.
/// </summary>
let funcPointFree x l = List.map ((*) x) l

/// <summary>
/// Pipeline version using |> operator to pass the list.
/// </summary>
let funcPipeline x l = l |> List.map ((*) x)
// <copyright file="CountEven.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

module CountEven

/// <summary>
/// Counts even numbers using map and sum.
/// Maps each number to 1 if even, 0 otherwise, then sums.
/// </summary>
let countEvenWithMap (list: int list) =
    list |> List.map (fun x -> if x % 2 = 0 then 1 else 0) |> List.sum

/// <summary>
/// Counts even numbers using filter and length.
/// Filters even numbers and returns the length of the result.
/// </summary>
let countEvenWithFilter (list: int list) =
    list |> List.filter (fun x -> x % 2 = 0) |> List.length

/// <summary>
/// Counts even numbers using fold.
/// Folds over the list, incrementing accumulator for each even number.
/// </summary>
let countEvenWithFold (list: int list) =
    list |> List.fold (fun acc x -> if x % 2 = 0 then acc + 1 else acc) 0
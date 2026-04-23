// <copyright file="ListIndexOf.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

namespace ListIndexOf

/// <summary>
/// Finds the first position (0-based index) of the given value in the list.
/// Returns None if the value is not found.
/// </summary>
/// <param name="value">The value to search for</param>
/// <param name="list">The list to search in</param>
/// <returns>Some index if found, None otherwise</returns>
module ListIndexOf =
    let findIndex value list =
        let rec loop idx lst =
            match lst with
            | [] -> None
            | head :: tail ->
                if head = value then Some idx
                else loop (idx + 1) tail
        loop 0 list
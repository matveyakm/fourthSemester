namespace ListMin

module MinFinder =
    /// <summary>
    /// Finds the smallest element in a list without using recursion or List.min.
    /// Uses List.reduce which provides functional iteration without imperative constructs.
    /// </summary>
    /// <param name="list">The input list to find the minimum element in.</param>
    /// <returns>The smallest element in the list.</returns>
    let findMin list =
        List.reduce (fun acc x -> if acc < x then acc else x) list

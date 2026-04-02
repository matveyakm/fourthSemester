namespace PrintSquare

module Square =
    /// <summary>
    /// Creates a row string: either full of stars or stars with spaces
    /// </summary>
    /// <param name="isBorder">Whether this is a border row (first or last)</param>
    /// <param name="n">The side length of the square</param>
    let createRow isBorder n =
        if isBorder || n = 1 then
            String.replicate n "*"
        else
            "*" + String.replicate (n - 2) " " + "*"

    /// <summary>
    /// Recursively builds all rows of the square
    /// </summary>
    let rec buildRows n rowIndex =
        if rowIndex < n then
            let isBorder = rowIndex = 0 || rowIndex = n - 1
            createRow isBorder n :: buildRows n (rowIndex + 1)
        else
            []

    /// <summary>
    /// Returns the square as a list of strings
    /// </summary>
    /// <param name="n">The side length of the square</param>
    let getSquare n =
        if n <= 0 then [] else buildRows n 0

    /// <summary>
    /// Prints a square of stars with side n to console
    /// </summary>
    /// <param name="n">The side length of the square</param>
    let printSquare n =
        getSquare n |> List.iter (fun s -> printfn "%s" s)
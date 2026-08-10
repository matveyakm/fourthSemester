// <copyright file="PhoneBookIO.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

namespace PhoneList

open System.IO

/// <summary>
/// Module for file I/O operations on phone book data.
/// </summary>
module PhoneBookIO =

    /// <summary>
    /// Parses a single line into (name, phone) tuple.
    /// Format: name;phone
    /// </summary>
    let parseLine (line: string) =
        match line.Split(';') with
        | [| name; phone |] -> Some (name.Trim(), phone.Trim())
        | _ -> None

    /// <summary>
    /// Formats a (name, phone) tuple into a single line.
    /// Format: name;phone
    /// </summary>
    let formatLine (name, phone) =
        sprintf "%s;%s" name phone

    /// <summary>
    /// Loads phone book from file.
    /// Returns Error on failure, Ok PhoneBook on success.
    /// </summary>
    let load (filePath: string) =
        try
            if not (File.Exists filePath) then
                Ok PhoneBookData.empty
            else
                let lines = File.ReadAllLines(filePath)
                let entries =
                    lines
                    |> Array.choose parseLine
                    |> Array.toList
                
                let rec addEntries (entries: (string * string) list) pb =
                    match entries with
                    | [] -> pb
                    | (name, phone) :: rest ->
                        match pb |> PhoneBook.add name phone with
                        | Ok newPb -> addEntries rest newPb
                        | Error _ -> addEntries rest pb
                
                Ok (addEntries entries PhoneBookData.empty)
        with
        | ex -> Error (ex.Message)

    /// <summary>
    /// Saves phone book to file.
    /// Returns Error on failure, Ok unit on success.
    /// </summary>
    let save (filePath: string) (pb: PhoneBookData) =
        try
            let lines =
                pb
                |> PhoneBook.toList
                |> List.map formatLine
                |> Array.ofList
            File.WriteAllLines(filePath, lines)
            Ok ()
        with
        | ex -> Error (ex.Message)
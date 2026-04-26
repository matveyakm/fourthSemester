// <copyright file="PhoneBookUI.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

namespace PhoneList

open System

/// <summary>
/// Interactive CLI user interface for phone book operations.
/// </summary>
module PhoneBookUI =

    let private printMenu () =
        printfn "==================================="
        printfn "  Phone Book - Menu"
        printfn "==================================="
        printfn "1. Add entry (name and phone)"
        printfn "2. Find phone by name"
        printfn "3. Find name by phone"
        printfn "4. List all entries"
        printfn "5. Save to file"
        printfn "6. Load from file"
        printfn "0. Exit"
        printfn "==================================="

    let rec private runLoop (pb: PhoneBookData) filePath =
        printMenu ()
        printf "Choose an option: "
        let choice = Console.ReadLine()
        
        match choice with
        | "0" ->
            printfn "Goodbye!"
            pb
        | "1" ->
            printf "Enter name: "
            let name = Console.ReadLine()
            printf "Enter phone: "
            let phone = Console.ReadLine()
            
            match pb |> PhoneBook.add name phone with
            | Ok newPb ->
                printfn "Entry added successfully."
                runLoop newPb filePath
            | Error msg ->
                printfn "Error: %s" msg
                runLoop pb filePath
        | "2" ->
            printf "Enter name to search: "
            let name = Console.ReadLine()
            
            match pb |> PhoneBook.findPhoneByName name with
            | Some phone -> printfn "Phone: %s" phone
            | None -> printfn "Name not found."
            
            runLoop pb filePath
        | "3" ->
            printf "Enter phone to search: "
            let phone = Console.ReadLine()
            
            match pb |> PhoneBook.findNameByPhone phone with
            | Some name -> printfn "Name: %s" name
            | None -> printfn "Phone not found."
            
            runLoop pb filePath
        | "4" ->
            let entries = pb |> PhoneBook.toList
            if List.isEmpty entries then
                printfn "Phone book is empty."
            else
                printfn "Phone Book Contents:"
                printfn "---------------------"
                entries
                |> List.iter (fun (name, phone) ->
                    printfn "%s -> %s" name phone)
            
            runLoop pb filePath
        | "5" ->
            match PhoneBookIO.save filePath pb with
            | Ok () -> printfn "Saved to %s" filePath
            | Error msg -> printfn "Error saving: %s" msg
            
            runLoop pb filePath
        | "6" ->
            match PhoneBookIO.load filePath with
            | Ok newPb ->
                printfn "Loaded from %s" filePath
                runLoop newPb filePath
            | Error msg ->
                printfn "Error loading: %s" msg
                runLoop pb filePath
        | _ ->
            printfn "Invalid option. Try again."
            runLoop pb filePath

    /// <summary>
    /// Starts the interactive phone book application.
    /// Default file path is "phonebook.txt".
    /// </summary>
    let run () =
        let defaultFile = "phonebook.txt"
        printfn "Welcome to Phone Book!"
        printfn "Using default file: %s" defaultFile
        runLoop PhoneBookData.empty defaultFile
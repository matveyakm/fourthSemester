// <copyright file="PhoneBook.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

namespace PhoneList

/// <summary>
/// Immutable phone book data structure holding names and phone numbers.
/// Uses two maps for bidirectional lookup: by name and by phone.
/// </summary>
type PhoneBookData =
    { nameToPhone: Map<string, string>
      phoneToName: Map<string, string> }
    static member empty =
        { nameToPhone = Map.empty
          phoneToName = Map.empty }

/// <summary>
/// Module providing phone book operations.
/// All functions return new immutable PhoneBook instances.
/// </summary>
module PhoneBook =

    /// <summary>
    /// Checks if name exists in the phone book.
    /// </summary>
    let hasName (name: string) (pb: PhoneBookData) =
        pb.nameToPhone |> Map.containsKey name

    /// <summary>
    /// Checks if phone exists in the phone book.
    /// </summary>
    let hasPhone (phone: string) (pb: PhoneBookData) =
        pb.phoneToName |> Map.containsKey phone

    /// <summary>
    /// Adds a new entry to the phone book.
    /// Returns Error if name or phone already exists.
    /// </summary>
    let add (name: string) (phone: string) (pb: PhoneBookData) =
        if pb |> hasName name then
            Error "Name already exists"
        elif pb |> hasPhone phone then
            Error "Phone already exists"
        else
            let newPb =
                { nameToPhone = pb.nameToPhone |> Map.add name phone
                  phoneToName = pb.phoneToName |> Map.add phone name }
            Ok newPb

    /// <summary>
    /// Finds phone number by name.
    /// Returns None if name not found.
    /// </summary>
    let findPhoneByName (name: string) (pb: PhoneBookData) =
        pb.nameToPhone |> Map.tryFind name

    /// <summary>
    /// Finds name by phone number.
    /// Returns None if phone not found.
    /// </summary>
    let findNameByPhone (phone: string) (pb: PhoneBookData) =
        pb.phoneToName |> Map.tryFind phone

    /// <summary>
    /// Returns all entries as a list of (name, phone) tuples.
    /// </summary>
    let toList (pb: PhoneBookData) =
        pb.nameToPhone |> Map.toList

    /// <summary>
    /// Returns the number of entries in the phone book.
    /// </summary>
    let count (pb: PhoneBookData) =
        pb.nameToPhone |> Map.count
// <copyright file="PhoneBookTests.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

namespace PhoneList.Tests

open NUnit.Framework
open FsUnit
open PhoneList

module PhoneBookTests =

    /// <summary>
    /// Tests that an empty phone book has zero entries.
    /// </summary>
    [<Test>]
    let EmptyPhoneBookShouldHaveZeroEntries () =
        let pb = PhoneBookData.empty
        pb |> PhoneBook.count |> should equal 0

    /// <summary>
    /// Tests that adding an entry increases the count by one.
    /// </summary>
    [<Test>]
    let AddEntryShouldIncreaseCount () =
        let pb = PhoneBookData.empty
        let newPb = 
            match pb |> PhoneBook.add "Alice" "123-456-7890" with
            | Ok x -> x
            | Error _ -> failwith "Should have succeeded"
        newPb |> PhoneBook.count |> should equal 1

    /// <summary>
    /// Tests that adding multiple entries increases count accordingly.
    /// </summary>
    [<Test>]
    let AddMultipleEntriesShouldIncreaseCount () =
        let pb = PhoneBookData.empty
        let pb1 = 
            match pb |> PhoneBook.add "Alice" "123-456-7890" with Ok x -> x | _ -> failwith ""
        let pb2 = 
            match pb1 |> PhoneBook.add "Bob" "987-654-3210" with Ok x -> x | _ -> failwith ""
        let pb3 = 
            match pb2 |> PhoneBook.add "Charlie" "555-555-5555" with Ok x -> x | _ -> failwith ""
        pb3 |> PhoneBook.count |> should equal 3

    /// <summary>
    /// Tests that adding a duplicate name returns an error.
    /// </summary>
    [<Test>]
    let AddDuplicateNameShouldReturnError () =
        let pb = PhoneBookData.empty
        let pb1 = 
            match pb |> PhoneBook.add "Alice" "123-456-7890" with Ok x -> x | _ -> failwith ""
        match pb1 |> PhoneBook.add "Alice" "999-999-9999" with
        | Ok _ -> failwith "Should have returned error"
        | Error _ -> ()

    /// <summary>
    /// Tests that adding a duplicate phone returns an error.
    /// </summary>
    [<Test>]
    let AddDuplicatePhoneShouldReturnError () =
        let pb = PhoneBookData.empty
        let pb1 = 
            match pb |> PhoneBook.add "Alice" "123-456-7890" with Ok x -> x | _ -> failwith ""
        match pb1 |> PhoneBook.add "Bob" "123-456-7890" with
        | Ok _ -> failwith "Should have returned error"
        | Error _ -> ()

    /// <summary>
    /// Tests that finding a phone by name returns the correct phone number.
    /// </summary>
    [<Test>]
    let FindPhoneByNameShouldReturnCorrectPhone () =
        let pb = PhoneBookData.empty
        let pb1 = 
            match pb |> PhoneBook.add "Alice" "123-456-7890" with Ok x -> x | _ -> failwith ""
        let pb2 = 
            match pb1 |> PhoneBook.add "Bob" "987-654-3210" with Ok x -> x | _ -> failwith ""
        pb2 |> PhoneBook.findPhoneByName "Alice" |> should equal (Some "123-456-7890")

    /// <summary>
    /// Tests that finding a phone by non-existent name returns None.
    /// </summary>
    [<Test>]
    let FindPhoneByNameShouldReturnNoneForNonExistent () =
        let pb = PhoneBookData.empty
        pb |> PhoneBook.findPhoneByName "Unknown" |> should equal None

    /// <summary>
    /// Tests that finding a name by phone returns the correct name.
    /// </summary>
    [<Test>]
    let FindNameByPhoneShouldReturnCorrectName () =
        let pb = PhoneBookData.empty
        let pb1 = 
            match pb |> PhoneBook.add "Alice" "123-456-7890" with Ok x -> x | _ -> failwith ""
        let pb2 = 
            match pb1 |> PhoneBook.add "Bob" "987-654-3210" with Ok x -> x | _ -> failwith ""
        pb2 |> PhoneBook.findNameByPhone "987-654-3210" |> should equal (Some "Bob")

    /// <summary>
    /// Tests that finding a name by non-existent phone returns None.
    /// </summary>
    [<Test>]
    let FindNameByPhoneShouldReturnNoneForNonExistent () =
        let pb = PhoneBookData.empty
        pb |> PhoneBook.findNameByPhone "000-000-0000" |> should equal None

    /// <summary>
    /// Tests that toList returns all entries as a list.
    /// </summary>
    [<Test>]
    let ToListShouldReturnAllEntries () =
        let pb = PhoneBookData.empty
        let pb1 = 
            match pb |> PhoneBook.add "Alice" "123-456-7890" with Ok x -> x | _ -> failwith ""
        let pb2 = 
            match pb1 |> PhoneBook.add "Bob" "987-654-3210" with Ok x -> x | _ -> failwith ""
        let entries = pb2 |> PhoneBook.toList
        entries |> List.length |> should equal 2
        entries |> should contain ("Alice", "123-456-7890")
        entries |> should contain ("Bob", "987-654-3210")

    /// <summary>
    /// Tests that hasName returns true for existing name.
    /// </summary>
    [<Test>]
    let HasNameShouldReturnTrueForExistingName () =
        let pb = PhoneBookData.empty
        let pb1 = 
            match pb |> PhoneBook.add "Alice" "123-456-7890" with Ok x -> x | _ -> failwith ""
        pb1 |> PhoneBook.hasName "Alice" |> should equal true

    /// <summary>
    /// Tests that hasName returns false for non-existing name.
    /// </summary>
    [<Test>]
    let HasNameShouldReturnFalseForNonExistingName () =
        let pb = PhoneBookData.empty
        pb |> PhoneBook.hasName "Unknown" |> should equal false

    /// <summary>
    /// Tests that hasPhone returns true for existing phone.
    /// </summary>
    [<Test>]
    let HasPhoneShouldReturnTrueForExistingPhone () =
        let pb = PhoneBookData.empty
        let pb1 = 
            match pb |> PhoneBook.add "Alice" "123-456-7890" with Ok x -> x | _ -> failwith ""
        pb1 |> PhoneBook.hasPhone "123-456-7890" |> should equal true

    /// <summary>
    /// Tests that hasPhone returns false for non-existing phone.
    /// </summary>
    [<Test>]
    let HasPhoneShouldReturnFalseForNonExistingPhone () =
        let pb = PhoneBookData.empty
        pb |> PhoneBook.hasPhone "000-000-0000" |> should equal false

    /// <summary>
    /// Tests that the phone book is immutable - adding does not modify original.
    /// </summary>
    [<Test>]
    let PhoneBookIsImmutableAddDoesNotModifyOriginal () =
        let pb = PhoneBookData.empty
        let pb1 = 
            match pb |> PhoneBook.add "Alice" "123-456-7890" with Ok x -> x | _ -> failwith ""
        pb |> PhoneBook.count |> should equal 0
        pb1 |> PhoneBook.count |> should equal 1
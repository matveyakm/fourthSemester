namespace HashTableTests

module Tests =

    open NUnit.Framework
    open FsUnit

    /// <summary>
    /// Simple hash function for integers
    /// </summary>
    let intHash (x: int) = abs x

    /// <summary>
    /// Simple hash function for strings
    /// </summary>
    let stringHash (s: string) = s.GetHashCode()

    /// <summary>
    /// Test adding element increases count
    /// </summary>
    [<Test>]
    let AddIncreasesCount() =
        let hashTable = HashTableLib.HashTable.create intHash
        hashTable.Add(1)
        hashTable.Count |> should equal 1

    /// <summary>
    /// Test adding duplicate element does not increase count
    /// </summary>
    [<Test>]
    let AddDuplicateDoesNotIncreaseCount() =
        let hashTable = HashTableLib.HashTable.create intHash
        hashTable.Add(1)
        hashTable.Add(1)
        hashTable.Count |> should equal 1

    /// <summary>
    /// Test contains returns true for added element
    /// </summary>
    [<Test>]
    let ContainsReturnsTrueForAddedElement() =
        let hashTable = HashTableLib.HashTable.create intHash
        hashTable.Add(42)
        hashTable.Contains(42) |> should be True

    /// <summary>
    /// Test contains returns false for non-added element
    /// </summary>
    [<Test>]
    let ContainsReturnsFalseForNonAddedElement() =
        let hashTable = HashTableLib.HashTable.create intHash
        hashTable.Add(42)
        hashTable.Contains(100) |> should be False

    /// <summary>
    /// Test contains works on empty table
    /// </summary>
    [<Test>]
    let ContainsWorksOnEmptyTable() =
        let hashTable = HashTableLib.HashTable.create intHash
        hashTable.Contains(1) |> should be False

    /// <summary>
    /// Test remove returns true and decreases count for existing element
    /// </summary>
    [<Test>]
    let RemoveExistingElement() =
        let hashTable = HashTableLib.HashTable.create intHash
        hashTable.Add(1)
        hashTable.Add(2)
        let result = hashTable.Remove(1)
        result |> should be True
        hashTable.Count |> should equal 1
        hashTable.Contains(1) |> should be False

    /// <summary>
    /// Test remove returns false for non-existing element
    /// </summary>
    [<Test>]
    let RemoveNonExistingElement() =
        let hashTable = HashTableLib.HashTable.create intHash
        hashTable.Add(1)
        let result = hashTable.Remove(999)
        result |> should be False
        hashTable.Count |> should equal 1

    /// <summary>
    /// Test remove works on empty table
    /// </summary>
    [<Test>]
    let RemoveFromEmptyTable() =
        let hashTable = HashTableLib.HashTable.create intHash
        let result = hashTable.Remove(1)
        result |> should be False

    /// <summary>
    /// Test add, contains, remove operations with strings
    /// </summary>
    [<Test>]
    let WorkWithStrings() =
        let hashTable = HashTableLib.HashTable.create stringHash
        hashTable.Add("hello")
        hashTable.Add("world")
        
        hashTable.Contains("hello") |> should be True
        hashTable.Contains("world") |> should be True
        hashTable.Contains("foo") |> should be False
        
        hashTable.Remove("hello") |> should be True
        hashTable.Contains("hello") |> should be False
        hashTable.Contains("world") |> should be True

    /// <summary>
    /// Test multiple add operations
    /// </summary>
    [<Test>]
    let AddMultipleElements() =
        let hashTable = HashTableLib.HashTable.create intHash
        for i in 1..10 do
            hashTable.Add(i)
        hashTable.Count |> should equal 10

    /// <summary>
    /// Test using createWithCapacity function
    /// </summary>
    [<Test>]
    let CreateWithCapacity() =
        let hashTable = HashTableLib.HashTable.createWithCapacity intHash 32
        hashTable.Add(1)
        hashTable.Add(2)
        hashTable.Count |> should equal 2

namespace HashTableLib

open System

/// <summary>
/// Hash table implementation with configurable hash function.
/// Supports Add, Contains, and Remove operations.
/// </summary>
/// <typeparam name="'T">Type of elements stored in the hash table</typeparam>
type HashTable<'T when 'T: equality>(hashFunction: 'T -> int, initialCapacity: int) =
    
    let mutable size = 0
    let mutable capacity = max initialCapacity 16
    let loadFactor = 0.75
    
    let mutable buckets: 'T list array = Array.create capacity []
    
    /// <summary>
    /// Resizes the internal array when the load factor exceeds the threshold.
    /// </summary>
    member private this.Resize() =
        let newCapacity = capacity * 2
        let newBuckets: 'T list array = Array.create newCapacity []
        
        for bucket in buckets do
            for item in bucket do
                let index = (hashFunction item) % newCapacity |> abs
                newBuckets[index] <- item :: newBuckets[index]
        
        buckets <- newBuckets
        capacity <- newCapacity
    
    /// <summary>
    /// Adds an element to the hash table. If the element already exists, it will not be duplicated.
    /// </summary>
    member this.Add(element: 'T) =
        if not (this.Contains(element)) then
            let index = (hashFunction element) % capacity |> abs
            buckets.[index] <- element :: buckets.[index]
            size <- size + 1
            
            if float size > float capacity * loadFactor then
                this.Resize()
    
    /// <summary>
    /// Checks whether an element exists in the hash table.
    /// </summary>
    member this.Contains(element: 'T): bool =
        let index = (hashFunction element) % capacity |> abs
        List.exists ((=) element) buckets.[index]
    
    /// <summary>
    /// Removes an element from the hash table if it exists.
    /// </summary>
    member this.Remove(element: 'T): bool =
        if this.Contains(element) then
            let index = (hashFunction element) % capacity |> abs
            buckets.[index] <- List.filter ((<>) element) buckets.[index]
            size <- size - 1
            true
        else
            false
    
    /// <summary>
    /// Returns the number of elements in the hash table.
    /// </summary>
    member this.Count: int = size

/// <summary>
/// Module containing factory functions for creating hash tables.
/// </summary>
module HashTable =
    
    /// <summary>
    /// Creates a new hash table with the given hash function and default capacity.
    /// </summary>
    let create hashFunction = HashTable(hashFunction, 16)
    
    /// <summary>
    /// Creates a new hash table with the given hash function and specified initial capacity.
    /// </summary>
    let createWithCapacity hashFunction capacity = HashTable(hashFunction, capacity)

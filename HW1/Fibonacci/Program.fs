// <copyright file="Program.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

module Program

open System.Numerics
open Fibonacci

printfn "Fibonacci sequence (0-19):"
for i = 0 to 19 do
    printfn "Fib(%d) = %s" i (string (fib i))

printfn ""
printfn "Fib(30) = %s" (string (fib 30))
printfn "Fib(40) = %s" (string (fib 40))
printfn "Fib(50) = %s" (string (fib 50))
printfn "Fib(100) = %s" (string (fib 100))
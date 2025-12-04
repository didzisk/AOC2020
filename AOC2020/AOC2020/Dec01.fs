module Dec01

open System
open System.IO

let filename = MetaUtils.getTodayInput 1

let inputStrings =
    File.ReadAllLines filename
    |> Array.map int64

let Calc1 (a:int64 array) =
    for i = 0 to a.Length-1 do
        for j = i to a.Length-1 do
                if a.[i] + a.[j] = 2020 then
                    printfn "Part1 %d" (a.[i] * a.[j])

let Calc2 (a: int64 array) =
    for i = 0 to a.Length-1 do
        for j = i to a.Length-1 do
            for k = j to a.Length-1 do
                if a.[i] + a.[j] + a.[k] = 2020 then
                    printfn "Part2 %d" (a.[i] * a.[j] * a.[k])
                    
let Part1 ()=
    inputStrings
    |> Calc1
    inputStrings
    |> Calc2
    
                        
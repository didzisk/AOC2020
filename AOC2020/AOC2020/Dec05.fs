module Dec05

open System
open System.IO

let filename = MetaUtils.getTodayInput 5

let inputStrings =
    File.ReadAllLines filename

let createSeat2 (line:string) =
    line.ToCharArray()
    |> Array.mapi (
        fun i x -> 
            let mask = 0b1000000000 >>> i
            if (x='B' || x='R') then
                mask
            else
                0
        )
    |> Array.sum

let bitmaskToChar id mask =
    if id &&& mask = mask then
        if mask>7 then
            'R'
        else
            'B'
    else
        if mask>7 then
            'L'
        else
            'F'

let idToSeat (id:int) =
    seq {
        for i= 0 to 9 do
            let mask = 0b1000000000 >>> i
            yield bitmaskToChar id mask
            }
    |> Array.ofSeq
    |> String
    
let Calc () =
    inputStrings
    |> Array.map createSeat2
    |> Array.max
    |> printfn "maxSeatId1 : %d"

    // inputStrings
    // |> Array.map createSeat2
    // |> Array.sort
    // |> Array.mapi (fun i x -> (i+89), x)
    // |> Array.filter (fun (x,y) -> x<y)
    // |> Array.map (fun (x,y)->x)
    // |> Array.min
    // |> printfn "*************missing %d"
    
    inputStrings
    |> Array.map createSeat2
    |> Array.sort
    |> Array.pairwise
    |> Array.filter (fun (a,b) -> b-a>1)
    |> Array.map (fun (a,_)->a+1)
    |> Array.iter (printfn "Part 2 candidate (choose one not in front or back): %d")
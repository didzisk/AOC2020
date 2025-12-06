module Dec09

open System.IO

let filename = MetaUtils.getTodayInput 9

let inputStrings =
    File.ReadAllLines filename

let inputLines = inputStrings |> Array.map int64

let preamble (start:int) (prlen:int) (lines:int64 array) =
    Array.sub lines (start-prlen) prlen
    |> Set.ofArray
    |> Set.toArray

let preambleSums preamble =
    Array.allPairs preamble preamble
    |> Array.map (fun (a,b)-> a+b)
    |> Set.ofArray

let validElm (i:int) (prlen:int) (lines:int64 array) =
    if i<prlen then
        -1
    else
        let pr = preamble i prlen lines
        let sums = (preambleSums pr)
        if Set.contains lines.[i] sums then 
            1
        else
            0

let calc1 (prlen:int) (arr:int64 array) =

    arr
    |> Array.mapi (fun i x -> (validElm i prlen arr, x))
    |> Array.filter (fun (x,_) -> x=0)
    |> Array.map snd
    |> Array.head

let subarray lines start len =
    Array.sub lines start len

let calc2sum i j (lines:int64 array) =
    subarray lines i j
    |> Array.sum

let calcSums (lines:int64[]) =
    let len = Array.length lines
    seq {
        for i = 0 to len-1 do
            for j = 1 to len-1-i do
                yield ((calc2sum i j lines), i, j)
    }

let calc2 lines expected =
    
    lines
    |> calcSums
    |> Seq.filter (fun (a,i,j) -> a=expected)
    |> Seq.map (fun (a,i,j) -> 
        let sortedSub=
            subarray lines i j
            |> Array.sort
        let slen = Array.length sortedSub
        sortedSub.[0]+sortedSub.[slen-1]
        )
    |> Seq.head

let Calc ()=
    let a = calc1 25 inputLines
    a |> printfn "Part 1: %d"

    calc2 inputLines a
    |> printfn "Part 2: %d"
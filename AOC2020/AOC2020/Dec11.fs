module Dec11

open System
open System.IO

let filename = MetaUtils.getTodayInput 11

let inputStrings =
    File.ReadAllLines filename
    
let numNeighbors1 (arr:char array array) r0 c0 =
    seq{
        for dr,dc in ArrayUtils.dirsX do
            let r,c = r0+dr, c0+dc
            if ArrayUtils.validPos arr (r,c) && arr[r][c] = '#' then
                yield 1
        }
    |> Seq.length

let rec goDir (arr:char array array) r0 c0 dr dc =
    let r,c = r0+dr, c0+dc
    if not (ArrayUtils.validPos arr (r,c)) then
        0
    else if arr[r][c] = 'L' then
             0
    else if arr[r][c] = '#' then
             1
    else
        goDir arr r c dr dc 
    
let numNeighbors2 (arr:char array array) r0 c0 =
    seq{
        for dr,dc in ArrayUtils.dirsX do
            yield goDir arr r0 c0 dr dc
        }
    |> Seq.sum
    
let rec calc1 displayFunc (arr:char array array) =
    let next = Array.init arr.Length (fun r-> Array.init arr[r].Length (fun _->'.'))
    for r0 in 0..arr.Length-1 do
        for c0 in 0..arr[r0].Length-1 do
            let numNeighbors = numNeighbors1 arr r0 c0
            if numNeighbors = 0 && arr[r0][c0] = 'L' then
                next[r0][c0]<-'#'
            else if numNeighbors >= 4 && arr[r0][c0] = '#' then
                next[r0][c0]<-'L'
            else
                next[r0][c0]<-arr[r0][c0]
    displayFunc next
    if ArrayUtils.arraysEqual arr next then
        arr
        |> ArrayUtils.allWhere (fun x-> x='#')
        |> Seq.length
    else
        calc1 displayFunc next

let rec calc2 displayFunc (arr:char array array) =
    let next = Array.init arr.Length (fun r-> Array.init arr[r].Length (fun _->'.'))
    for r0 in 0..arr.Length-1 do
        for c0 in 0..arr[r0].Length-1 do
            let numNeighbors = numNeighbors2 arr r0 c0
            if numNeighbors = 0 && arr[r0][c0] = 'L' then
                next[r0][c0]<-'#'
            else if numNeighbors >= 5 && arr[r0][c0] = '#' then
                next[r0][c0]<-'L'
            else
                next[r0][c0]<-arr[r0][c0]
    displayFunc next
    if ArrayUtils.arraysEqual arr next then
        arr
        |> ArrayUtils.allWhere (fun x-> x='#')
        |> Seq.length
    else
        calc2 displayFunc next
    
let display (arr:char array array) =
    Threading.Thread.Sleep(1000)
    Console.Clear()
    for r0 in 0..arr.Length-1 do
        printfn "%s" (String(arr[r0]))

let displayDummy _ = ()        

let Calc() =
    
    let ex =
        [|
            "L.LL.LL.LL"
            "LLLLLLL.LL"
            "L.L.L..L.."
            "LLLL.LL.LL"
            "L.LL.LL.LL"
            "L.LLLLL.LL"
            "..L.L....."
            "LLLLLLLLLL"
            "L.LLLLLL.L"
            "L.LLLLL.LL"
        |]
    ex
    |> Array.map _.ToCharArray()
    |> calc1 display
    |> printfn "Part 1 ex: %d"

    inputStrings
    |> Array.map _.ToCharArray()
    |> calc1 displayDummy
    |> printfn "Part 1: %d"

    ex
    |> Array.map _.ToCharArray()
    |> calc2 display
    |> printfn "Part 2 ex: %d"

    inputStrings
    |> Array.map _.ToCharArray()
    |> calc2 displayDummy
    |> printfn "Part 2: %d"

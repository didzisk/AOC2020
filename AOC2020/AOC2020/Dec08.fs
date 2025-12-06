module Dec08

open System.IO
open Dec08Machine
open StringUtils

let filename = MetaUtils.getTodayInput 8

let inputStrings =
    File.ReadAllLines filename
    
let calc1 lines =
    let ms:MachineState =
        {
            PC = 0;
            VisitedLocations = Set.empty |> Set.add 0;
            Program = lines;
            Acc = 0;
            RunState = Continue
        }
    let res = Run ms
    res.Acc

let rec calc2 place (lines:string array) =
    let line = split " " lines.[place]
    let cmd = line.[0]
    let arg0 = line.[1]
    let newcmd = 
        match cmd with
        | "jmp" -> "nop"
        | "nop" -> "jmp"
        | a -> a

    let newline = newcmd + " " + arg0

    printfn "Changed %d from %A to %s" place line newline

    let newLines = Seq.toArray lines
    Array.set newLines place newline

    let st = 
        {
            PC = 0;
            VisitedLocations = Set.empty |> Set.add 0;
            Program = newLines;
            Acc = 0;
            RunState = Continue
        }

    let res = Run st
    if res.RunState = ReachedEnd then
        res.Acc
    else
        calc2 (place+1) lines 

let Calc()=
    inputStrings
    |> calc1
    |> printfn "Part1: %d"


    inputStrings
    |> calc2 1
    |> printfn "Part2: %d"

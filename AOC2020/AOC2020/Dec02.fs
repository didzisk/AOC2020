module Dec02

open System
open System.IO
open FParsec

let filename = MetaUtils.getTodayInput 2

let inputStrings =
    File.ReadAllLines filename
    
type Rule =
    {
        From:int
        To:int
        Char:char
        Password:string
    }

// 1-3 a: abcde
let pInterval: Parser<Rule, unit> = pipe4 (pint32 .>> pstring "-") (pint32 .>> spaces) (anyChar .>> pchar ':' .>> spaces) (restOfLine false)  (fun a b c d->{Rule.From = a; To = b; Char = c; Password = d})

let pwdValid1 (rule:Rule) =
    let n =
        rule.Password
        |> Seq.filter (fun x-> x=rule.Char)
        |> Seq.length
    n>=rule.From && n<=rule.To
    
let pwdValid2 (rule:Rule) =
    (rule.Password[rule.From-1] = rule.Char) <> (rule.Password[rule.To-1] = rule.Char) //here "<>" gives XOR  

let parsed = 
    inputStrings
    |> Seq.map (fun x ->
           match run pInterval x with
           | Success (rule, _, _) -> rule 
           | Failure(err, _, _) ->
                failwith $"Parsing '{x}' failed: {err}"
    )
//parsed |> Seq.iter (printfn "%A")
let parsedEx =
    ["1-3 a: abcde"
     "1-3 b: cdefg"
     "2-9 c: ccccccccc"]
    |> Seq.map (fun x ->
           match run pInterval x with
           | Success (rule, _, _) -> rule 
           | Failure(err, _, _) ->
                failwith $"Parsing '{x}' failed: {err}"
       )
 
    
let Calc () =
    parsed
    |> Seq.filter pwdValid1
    |> Seq.length
    |> printfn "Part1: %d"

    parsed
    |> Seq.filter pwdValid2
    |> Seq.length
    |> printfn "Part2: %d"
 
    parsedEx
    |> Seq.filter pwdValid2
    |> Seq.length
    |> printfn "Part2: %d"

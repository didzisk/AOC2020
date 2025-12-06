module Dec07

open System
open System.IO
open FParsec

let filename = MetaUtils.getTodayInput 7

let inputStrings =
    File.ReadAllLines filename

let pContainer:Parser<string,unit> = charsTillString " bags contain " true 80

let notASep = function
    | ',' -> false
    | '.' -> false
    | _ -> true
let pSep = (pchar ',' <|> pchar '.') .>> spaces

let pColoredContents = many (pint32 .>> spaces .>>. manySatisfy notASep .>> pSep)

//let pLine = pContainer .>>. (pEmptyContents <|> pColoredContents)

let pLine = pContainer .>>. pColoredContents

let parseLine input =
    let name, contents =
        match run pLine input with
        | Success(result, _, _) -> result
        | Failure(err, _, _)    -> failwith err //err, []
    let newContents =
        contents
        |> List.map (fun (n, s) ->
            let arr = s.Split ' '
            n, arr[0] + " " + arr[1]
            )
    let canHoldShinyGold =
        newContents
        |> List.exists (fun (_,c)->c="shiny gold")
    name, newContents, canHoldShinyGold 
    
let rec calcShinyGoldInner (arr:(string * (int32 * string) list * bool) array) =
    arr
    |> Array.map (fun (name, lines, b)->
        if b then
            name, lines, b
        else
            let subArr =
                arr
                |> Array.filter (fun (subName, _,_)-> lines |> List.exists(fun (_,x)->subName=x) )
            let newSubArr = calcShinyGoldInner subArr
            let newB =
                newSubArr
                |> Array.exists (fun (_,_,b)->b)
            name, lines, newB
        )

let rec calcShinyGold (acc:int) (arr:(string * (int32 * string) list * bool) array) =
    let newAcc =
        arr
        |> Array.filter (fun (_,_,b)->b)
        |> Array.length
    if newAcc = acc then
        arr
    else
        let newArr = arr |> calcShinyGoldInner
        calcShinyGold newAcc newArr         
        
let rec bagsInBag (arr:(string * (int32 * string) list * bool) array) (name:string) =
    let _,bags,_ = arr |> Array.find (fun (x,_,_)-> x = name)
    match bags with
    | [] -> 0
    | lbags ->
        let numInside =
            lbags
            |> List.map (fun (n,subName)-> n * ( bagsInBag arr subName + 1 ) )
            |> List.sum
        numInside

let Calc () =
    inputStrings
    |> Seq.iter (printfn "%s")

    let arr =
        inputStrings
        |> Array.map parseLine
    let newArr = arr |> calcShinyGold 0

    newArr
    |> Array.filter (fun (_,_,b)->b)
    |> Array.length
    |> printfn "Part 1: %d"

    let exArr =
        MetaUtils.todayFilenameEx 7
        |> File.ReadAllLines
        |> Array.map parseLine
        
    bagsInBag exArr "shiny gold"
    |> printfn "PartEx 2: %d"         
    
    bagsInBag arr "shiny gold"
    |> printfn "Part 2: %d"
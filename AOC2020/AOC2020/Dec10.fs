module Dec10

open System.IO

let filename = MetaUtils.getTodayInput 10

let inputStrings =
    File.ReadAllLines filename

let hops lines =
    lines
    |> List.pairwise
    |> List.map (fun (a,b)-> b-a)

let calc1 (lines: string array)=

    let slines = 0::(lines |> Seq.map int |> Seq.sort |> List.ofSeq)
    let shops = slines|> hops

    let c1 = shops |> List.filter (fun x->x=1) |> List.length
    let c3 = shops |> List.filter (fun x->x=3) |> List.length
    c1 * (c3 + 1)

let rec countways adapters start goal memo =
    let k=(List.length adapters, start)
    if Map.containsKey k memo then
        memo, memo.[k]
    else
        let ways0 = 
            if goal - start <= 3 then
                1L
            else
                0L
        let memo1, ways1 =
            match adapters with
            | head::tail ->
                let memo2,ways2 =
                    if head - start <=3 then
                        countways tail head goal memo
                    else
                        memo, 0L
                let memo3, ways3 = countways tail start goal memo2
                memo3, ways2+ways3
            | [] -> memo, 0L

        let ways = ways0 + ways1
        (Map.add k ways memo1), ways //return

let calc2 lines =
    let slines = lines |> Seq.map int |> Seq.sort |> List.ofSeq

    let goal = (List.max slines) + 3

    let _, ways = countways slines 0 goal Map.empty

    ways
    
let Calc () =
    calc1 inputStrings
    |> printfn "Part 1: %d"
    
    calc2 inputStrings
    |> printfn "Part 2: %d"
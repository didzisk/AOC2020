module Dec03

open System.IO

let filename = MetaUtils.getTodayInput 3

let inputStrings =
    File.ReadAllLines filename

let parseSize (lines:string seq) =
    let arr=lines |> Seq.toArray
    arr.Length (*rows*), arr.[0].Trim().Length (*cols*)

let rec downOneStep (grid:string[]) (rowsD:int) (colsR:int) currPos =
    let row, col, currCount = currPos
    let rows, cols = parseSize grid
    if row+rowsD>=rows then
        currCount
    else
        let nextRow, nextCol = 
            if (col+colsR >= cols) then
                row+rowsD, col+colsR-cols
            else
                row+rowsD, col+colsR

        if grid.[nextRow].[nextCol]='#' then
            downOneStep grid rowsD colsR (nextRow, nextCol, currCount+1)
        else
            downOneStep grid rowsD colsR (nextRow, nextCol, currCount)

let solve rows cols arr : int64 =
    downOneStep arr rows cols (0, 0, 0)
    |> int64
    
let solve2 arr=
    solve 1 1 arr
    * solve 1 3 arr
    * solve 1 5 arr
    * solve 1 7 arr
    * solve 2 1 arr
    |> printfn "Pt2 Solution: %d"

    
let Calc () =
    inputStrings
    |> solve 1 3
    |> printfn "Pt1 Solution D1 R3: %d"
    
    solve2 inputStrings

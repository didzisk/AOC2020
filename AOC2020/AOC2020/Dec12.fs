module Dec12

open System.IO

let filename = MetaUtils.getTodayInput 12

let inputStrings =
    File.ReadAllLines filename

type Dir =
    | N
    | E
    | S
    | W

let rec mod360 a=
    if a<0 then 
        mod360 (a+360)
    else
        if a>=360 then
            mod360 (a-360)
        else
            a

let degTodir deg =
    let newDeg = mod360 deg
    match newDeg with
    | 0-> E
    | 90 -> S
    | 180 -> W
    | 270 -> N
    | _ -> failwith "Unexpected direction"

let newDir (line:string) dir =
    let turn = line.[0]
    let tdeg = int (line.Substring(1))
    let deg = 
        match dir with
        | E -> 0
        | S -> 90
        | W -> 180
        | N -> 270
    match turn with
    | 'R' -> degTodir (deg+tdeg)
    | 'L' -> degTodir (deg-tdeg)
    | _ -> dir

let moveDist movedir units (x,y,dir) =
    match movedir with
    | E -> x+units, y, dir
    | S -> x, y+units, dir
    | W -> x-units, y, dir
    | N -> x, y-units, dir

let nextPos (x,y,dir) (line:string) =
    let cmd = line.[0]
    let arg = int (line.Substring(1))
    match cmd with
    | 'E' -> moveDist E arg (x,y,dir)
    | 'S' -> moveDist S arg (x,y,dir)
    | 'W' -> moveDist W arg (x,y,dir)
    | 'N' -> moveDist N arg (x,y,dir)
    | 'F' -> moveDist dir arg (x,y,dir)
    | 'R'| 'L' -> (x,y, (newDir line dir))

    | _ -> failwith (sprintf "unexpected command %s" line)

let finalPos lines =
    lines
    |> Seq.fold nextPos (0,0,E)

let mDistance (x:int,y:int,_)=
    (abs x) + (abs y)

let changeSpeed dir units (x,y) =
    match dir with
    | E -> x+units, y
    | S -> x, y-units
    | W -> x-units, y
    | N -> x, y+units

let flipSpeed deg (x,y) =
    let newDeg = mod360 deg
    match newDeg with
    | 0-> x,y
    | 90 -> y, -x
    | 180 -> -x, -y
    | 270 -> -y, x
    | _ -> failwith "Unexpected direction change"
    

let nextSpeed (vx, vy) (line:string) = 
    let cmd = line.[0]
    let arg = int (line.Substring(1))
    match cmd with
    | 'E' -> changeSpeed E arg (vx,vy)
    | 'S' -> changeSpeed S arg (vx,vy)
    | 'W' -> changeSpeed W arg (vx,vy)
    | 'N' -> changeSpeed N arg (vx,vy)
    | 'F' -> (vx,vy)
    | 'R' -> flipSpeed arg (vx, vy)
    | 'L' -> flipSpeed (-arg) (vx, vy)
    | _ -> failwith (sprintf "unexpected command %s" line)

let nextPos2 (x,y,vx,vy) (line:string) =
    let (vxn, vyn) = nextSpeed (vx, vy) line
    printfn $"X: {x}, Y:{y}, VX: {vx}, VY: {vy} CMD: {line}"
    let cmd = line.[0]
    let arg = int (line.Substring(1))
    let next=
        match cmd with
        | 'F' -> (x+vxn*arg, y+vyn*arg, vxn, vyn)
        | _ -> (x, y, vxn, vyn)
    next

let finalPos2 lines =
    lines
    |> Seq.fold nextPos2 (0,0,10,1)

let Calc() =
    inputStrings
    |> finalPos
    |> mDistance
    |> printfn "Part1 %A"

    inputStrings
    |> finalPos2
    |> fun (x,y,_a,_) -> x,y,_a 
    |> mDistance
    |> printfn "Part2 %A"

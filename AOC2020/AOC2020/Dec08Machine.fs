module Dec08Machine

open StringUtils

type TerminationReason=
    | Continue
    | ReachedEnd
    | LoopedBack

type MachineState = 
    {
        PC:int;
        Program:string[];
        VisitedLocations:Set<int>;
        Acc:int
        RunState: TerminationReason
    }

let private doAcc (state:MachineState) arg : MachineState =
    {state with
        PC=state.PC+1;
        Acc = state.Acc + arg;
    }

let private doJmp (state:MachineState) arg : MachineState =
    {state with
        PC=state.PC+arg;
    }

let private doNop(state:MachineState) arg : MachineState =
   {state with
       PC=state.PC+1;
   }

let rec private doOne state =
    let line = split " " state.Program.[state.PC]
    let cmd = line.[0]
    let arg0 = int32 line.[1]

    let middleState=
        match cmd with
            | "jmp" -> doJmp state arg0
            | "acc" -> doAcc state arg0
            | _ -> doNop state arg0

    let newState = 
        {middleState with 
            VisitedLocations = middleState.VisitedLocations.Add middleState.PC; 
        }
    if newState.PC >= state.Program.Length then
        {newState with RunState = ReachedEnd}
    else
        if state.VisitedLocations.Contains newState.PC then
            {newState with RunState = LoopedBack}
        else
            newState |> doOne  

let Run (state:MachineState) : MachineState =
    doOne state

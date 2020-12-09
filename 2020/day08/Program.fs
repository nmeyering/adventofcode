open System.IO

type Instruction = Acc of int | Jmp of int | Nop of int
type ProgramState = {
    Accumulator: int;
    ProgramCounter: int;
    Visited: int list;
}
type Program = Instruction []

let parseInstruction (instruction: string): Instruction =
    match instruction.Split " " with
    | [| "nop"; arg |] -> Nop (int arg)
    | [| "acc"; arg |] -> Acc (int arg)
    | [| "jmp"; arg |] -> Jmp (int arg)

let initialState = {
    Accumulator = 0;
    ProgramCounter = 0;
    Visited = []
}

let step (program: Program) (state: ProgramState): ProgramState =
    let instruction = program |> Array.item state.ProgramCounter
    if List.contains state.ProgramCounter state.Visited
    then { state with ProgramCounter = 1 + (Array.length program) }
    else
    match instruction with
    | Acc x -> { state with Visited = state.ProgramCounter :: state.Visited; Accumulator = state.Accumulator + x; ProgramCounter = state.ProgramCounter + 1; }
    | Jmp x -> { state with Visited = state.ProgramCounter :: state.Visited; ProgramCounter = state.ProgramCounter + x }
    | Nop _ -> { state with Visited = state.ProgramCounter :: state.Visited; ProgramCounter = state.ProgramCounter + 1 }

let program': Program = 
    [
    "nop +0"
    "acc +1"
    "jmp +4"
    "acc +3"
    "jmp -3"
    "acc -99"
    "acc +1"
    "jmp -4"
    "acc +6"
    ]
    |> List.map parseInstruction
    |> List.toArray

let program = File.ReadAllLines ("day08/input.txt") |> Seq.map parseInstruction |> Seq.toArray

let run (program: Program): ProgramState = 
    let rec run' program state =
        let state' = step program state
        if state'.ProgramCounter >= Array.length program
            then state'
            else run' program state'
        
    run' program initialState

let getAccumulator state = state.Accumulator

let answer1 = program |> run |> getAccumulator

let terminatedCorrectly program (state: ProgramState): bool =
    state.ProgramCounter = Array.length program

let swapInstruction (instruction: Instruction) =
    match instruction with
    | Acc x -> Acc x
    | Nop x -> Jmp x
    | Jmp x -> Nop x

let swapInstructionAti program i =
    let ret = Array.copy program
    Array.set ret i (swapInstruction (Array.item i program))
    ret

let answer2 =
    program
    |> Array.mapi (fun i _ -> swapInstructionAti program i)
    |> Array.find (fun p -> terminatedCorrectly p (run p))
    |> run |> getAccumulator
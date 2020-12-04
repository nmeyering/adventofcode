type Program = {
    Instructions: int list;
    Output: int -> unit;
    Input: int list
}

let writeAt i replacement xs =
    xs |> List.mapi (fun j old -> if j = i then replacement else old)

let digits' bas number =
    let rec f acc n =
        if n = 0 then
            acc
        else
            f ((n % bas) :: acc) (n / bas)
    f [] number

let digits = digits' 10

let opCode instr = instr % 100

let paramMode d instr =
    let rec digit d n =
        match d with
        | 0 -> n % 10
        | x when x < 0 -> failwith "must not be negative"
        | _ -> digit (d-1) (n/10)
    digit (d + 2) instr

let decodeOutputParam (program: int list) (instructionIndex: int) (paramIndex: int) =
    let param = program.Item (instructionIndex + 1 + paramIndex)
    let instruction = program.Item instructionIndex
    let mode = paramMode paramIndex instruction
    printfn "  out param %d, mode %d" param mode
    match mode with
        | 0 -> param
        | _ -> failwith "invalid output parameter mode"

let decodeParam (program: int list) (instructionIndex: int) (paramIndex: int) =
    let param = program.Item (instructionIndex + 1 + paramIndex)
    let instruction = program.Item instructionIndex
    let mode = paramMode paramIndex instruction
    let res = match mode with
                    | 0 -> program.Item param
                    | 1 -> param
                    | _ -> failwith "invalid parameter mode"
    printfn "      param %d, mode %d, value %d" param mode res
    res

let binOp (op: int -> int -> int) (program: int list) (index: int): (int list) =
    let x = decodeParam program index 0
    let y = decodeParam program index 1
    let output = decodeOutputParam program index 2
    program |> writeAt output (op x y)

let executeInstruction (program: Program) (index: int) : (int * Program) =
    let instr = program.Instructions.Item(index)
    let op = opCode instr
    // printfn "executing %A at index %A" instr index 
    let param x = decodeParam program.Instructions index x
    let outParam x = decodeOutputParam program.Instructions index x
    match op with
        | 1 -> let prog = binOp (+) program.Instructions index
               (index + 4, {program with Instructions = prog})
        | 2 -> let prog = binOp (*) program.Instructions index
               (index + 4, {program with Instructions = prog})
        | 3 -> let pos = outParam 0
               match program.Input with
               | [] -> failwith "input exhausted"
               | input :: rest ->
                   let prog = program.Instructions |> writeAt pos input
                   (index + 2, {program with Instructions = prog; Input = rest})
        | 4 -> param 0 |> program.Output
               (index + 2, program)
        | 5 -> let destination = if (param 0) <> 0 then param 1 else index + 3
               (destination, program)
        | 6 -> let destination = if (param 0) = 0 then param 1 else index + 3
               (destination, program)
        | 7 -> let prog = binOp (fun x y -> if x < y then 1 else 0) program.Instructions index
               (index + 4, {program with Instructions = prog})
        | 8 -> let prog = binOp (fun x y -> if x = y then 1 else 0) program.Instructions index
               (index + 4, {program with Instructions = prog})
        | 99 -> (-1, program)
        | _ -> failwithf "illegal instruction %d at index %d" op index

let execute (program: Program): Program =
    let len = List.length program.Instructions
    let rec loop pc prog =
        if pc < 0 || pc >= len then
            prog
        else
            let (pc', gram') = executeInstruction prog pc
            loop pc' gram'
    loop 0 program

type ProgramBuilder(program: int list) =
    member __.Return(input) = 
        let mutable result: int = 0
        execute {
            Instructions = program
            Input = input
            Output = fun o -> result <- o
        } |> ignore
        result

open System.IO

let xs = 
    File.ReadLines(@"day07/input.txt")
    |> Seq.head
    |> fun csv -> csv.Split ','
    |> Array.map int
    |> Array.toList


let runProgram = ProgramBuilder(xs)

let res = runProgram {
    return [4;0]
}

// let run = execute >> ignore

// // run program
// let phases = [0;1;2;3;4]

// run ({
//     Instructions = xs
//     Input = [phases.[0];0]
//     Output = fun a -> (run {
//         Instructions = xs
//         Input = [phases.[1];a]
//         Output = fun b -> (run {
//             Instructions = xs
//             Input = [phases.[2];b]
//             Output = fun c -> (run {
//                 Instructions = xs
//                 Input = [phases.[3];c]
//                 Output = fun d -> (run {
//                     Instructions = xs
//                     Input = [phases.[4];d]
//                     Output = printfn "e=%A"
//                 })
//         })
//         })
//     })
// })

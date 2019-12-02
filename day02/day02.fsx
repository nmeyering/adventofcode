open System.IO

let program = 
    File.ReadLines(@"day02/input.txt")
         |> Seq.head 
         |> fun csv -> csv.Split [|','|]
         |> Array.map int
         |> Array.toList

let writeAt i replacement xs =
    xs |> List.mapi (fun j old -> if j = i then replacement else old)

// let program = [1;0;0;0;99]
let fix1202 program = 
    program |> writeAt 1 12 |> writeAt 2 2

let binOp op (program: int list) index =
    let val1 = program.[program.[index + 1]]
    let val2 = program.[program.[index + 2]]
    let destIndex = program.Item(index + 3)
    program |> writeAt destIndex (op val1 val2)

let add = binOp (+)
let mul = binOp (*)

let step (program: int list) (index: int) : (int * int list)=
    match program.Item(index) with
        | 1 -> (index + 4, add program index)
        | 2 -> (index + 4, mul program index)
        | 99 -> (-1, program)
        | _ -> failwith "illegal instruction"

let execute (program: int list) =
    let len = List.length program
    let rec loop pc gram =
        if pc < 0 || pc >= len then
            gram
        else
            let (pc', gram') = step gram pc
            loop pc' gram'
    loop 0 program
                
let answer1 = program |> fix1202 |> execute |> List.head

let setInput noun verb =
    writeAt 1 noun >> writeAt 2 verb

let cartesian n f =
    List.collect (fun x -> List.map (fun y -> f x y) [0..n]) [0..n]

let answer2 =
    cartesian 99 (fun noun verb -> (noun, verb, program
                                                |> setInput noun verb 
                                                |> execute
                                                |> List.head))
    |> List.find (fun (_, _, h) -> h = 19690720)
    |> fun (noun, verb, _) -> 100 * noun + verb

printfn "answer 1: %A, answer 2: %A" answer1 answer2

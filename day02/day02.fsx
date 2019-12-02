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
                
let result = program |> fix1202 |> execute
printfn "%A" result

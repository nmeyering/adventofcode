open System.IO
open System

let input = File.ReadAllLines ("day14/input.txt") |> Seq.toList
// let input = [
//     "mask = 000000000000000000000000000000X1001X"
//     "mem[42] = 100"
//     "mask = 00000000000000000000000000000000X0XX"
//     "mem[26] = 1"
// ]

type Mem = {
    Address: int64
    Value: int64
}

type Instruction = Mask of char [] | Mem of Mem

let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None

let parseInstruction (line: string): Instruction option =
    match line with
    | Prefix "mask = " rest -> rest |> fun s -> s.ToCharArray () |> Mask |> Some
    | Prefix "mem[" rest -> match rest.Split "] = " with
                            | [| address; value |] -> Some (Mem { Address = int64 address; Value = int64 value })
                            | _ -> None
    | _ -> None

let rec pown64 (b: int) (e: int): int64 =
    match e with
    | 0 -> 1L
    | _ -> (int64 b) * pown64 b (e - 1)

let toBinary (x: int64) = Convert.ToString (x, 2) |> fun s -> s.PadLeft (36, '0')
let fromBinary (x: char[]) =
    x
    |> Array.mapi (fun i c -> if c = '1' then pown64 2 (35 - i) |> int64 else 0L)
    |> Array.sum

let masked (mask: char[]) (value: int64) =
    value
    |> toBinary
    |> fun v -> v.ToCharArray ()
    |> Array.mapi (fun i c ->
        match mask.[i] with
        | 'X' -> c
        | m -> m
    )
    |> fromBinary

let write memory mask { Address = address; Value = value } =
    match Map.tryFind address memory with
    | None -> Map.add address (masked mask value) memory
    | _ -> Map.change address (fun _ -> Some (masked mask value)) memory


let noChangeMask = (Array.init 36 (fun _ -> 'X'))

let run (program: Instruction list): Map<int64, int64> option =
    let rec execute (memory: Map<int64, int64>) (mask: char []) (program: Instruction list): Map<int64, int64> =
        match program with
            | [] -> memory
            | line :: rest ->
                match line with
                | Mask newMask -> execute memory newMask rest
                | Mem instr -> execute (write memory mask instr) mask rest

    Some <| execute Map.empty noChangeMask program

let sequenceOption l = 
    if Seq.contains None l then None
    else Some (Seq.map Option.get l)

let answer1 =
    input
    |> List.map parseInstruction
    |> sequenceOption
    |> Option.bind (Seq.toList >> run)
    |> Option.map (Map.fold (fun s _ t -> s + t) 0L)

let rec cartesian lstlst =
    match lstlst with
    | h::[] ->
        List.fold (fun acc elem -> [elem]::acc) [] h
    | h::t ->
        List.fold (fun cacc celem ->
            (List.fold (fun acc elem -> (elem::celem)::acc) [] h) @ cacc
            ) [] (cartesian t)
    | _ -> []

let replaceXs (address: char[]) (replacements: (int * char) list) =
    let res = Array.copy address
    List.iter (fun (index, replacement) ->
        Array.set res index replacement) replacements
    res

// "101010" "X1001X"
// =>
// "X1101X"
// =>
// [0; 5]
// =>
// [[(0, 0); (0, 1)]; [(5, 0); (5, 1)]]
// =>
// [[(0,0); (5,0)]
// [(0,0); (5,1)]
// [(0,1); (5,0)]
// [(0,1); (5,1)]]
// =>
// ["011010"
// "011011"
// "111010"
// "111011"]
let decode (mask: char[]) (address: char []): char [] list =
    let masked =
        address
        |> Array.mapi (fun i c ->
            match mask.[i] with
                | '0' -> c
                | '1' -> '1'
                | 'X' -> 'X'
                | _ -> failwith "invalid mask")
    let xPositions =
        masked
        |> Array.fold (fun (i, acc) c ->
            match c with
            | 'X' -> (1 + i, i :: acc)
            | _ -> (1 + i, acc)) (0, [])
        |> snd
    let combinations =
        xPositions
        |> List.map (fun pos -> [(pos, '0'); (pos, '1')])
        |> cartesian
    combinations
    |> List.map (replaceXs masked)

let run2 (program: Instruction list): Map<int64, int64> option =
    let maskAddress (mask: char[]) (address: int64): int64 list =
        address
        |> toBinary
        |> fun v -> v.ToCharArray ()
        |> decode mask
        |> List.map fromBinary

    let writeAll memory mask { Address = address; Value = value } =
        maskAddress mask address
        |> List.fold (fun m addr -> write m noChangeMask { Address = addr; Value = value }) memory

    let rec execute (memory: Map<int64, int64>) (mask: char []) (program: Instruction list): Map<int64, int64> =
        match program with
            | [] -> memory
            | line :: rest ->
                match line with
                | Mask newMask -> execute memory newMask rest
                | Mem instr -> execute (writeAll memory mask instr) mask rest

    Some <| execute Map.empty (Array.init 36 (fun _ -> 'X')) program

let answer2 =
    input
    |> List.map parseInstruction
    |> sequenceOption
    |> Option.bind (Seq.toList >> run2)
    |> Option.map (Map.fold (fun s _ t -> s + t) 0L)
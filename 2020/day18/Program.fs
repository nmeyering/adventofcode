open System
open System.IO

type Token = Val of bigint | Plus | Times | Open | Close

let tokenize (s: string): Token list =
    let tokenize' (state: Token list) (c: char) =
        match c with
        | c when c >= '0' && c <= '9' ->
            Val (bigint (int c - int '0')) :: state
        | c when c = '+' -> Plus :: state
        | c when c = '*' -> Times :: state
        | c when c = '(' -> Open :: state
        | c when c = ')' -> Close :: state
        | _ -> state
    s
    |> (fun (s: string) -> s.ToCharArray ())
    |> Array.toList
    |> List.fold tokenize' []
    |> List.rev

let eval (expr: Token list): bigint option =
    let rec eval' (stack: Token list) (input: Token list): bigint option =
        // printfn "stack: %A" stack
        match input with
        | [] ->
            match stack with
            | [Val result] -> Some result
            | _ -> None
        | Val b :: rest ->
            match stack with
            | Plus :: Val a :: restStack -> eval' (Val (a + b) :: restStack) rest 
            | Times :: Val a :: restStack -> eval' (Val (a * b) :: restStack) rest 
            | _ -> eval' (Val b :: stack) rest
        | Plus :: rest -> eval' (Plus :: stack) rest
        | Times :: rest -> eval' (Times :: stack) rest
        | Open :: rest -> eval' (Open :: stack) rest
        | Close :: rest ->
            match stack with
            | Val a :: Open :: restStack -> eval' restStack (Val a :: rest)
            | _ -> None
    eval' [] expr

let input = File.ReadAllLines ("day18/input.txt") |> Seq.toList

let answer1 =
    input
    |> List.map (tokenize >> eval)
    |> List.reduce (Option.map2 (+))

let eval2 (expr: Token list): bigint option =
    let rec eval' (stack: Token list) (input: Token list): bigint option =
        match input with
        | [] ->
            match stack with
            | [Val result] -> Some result
            | [Val b; Times; Val a] -> eval' [Val (a * b)] []
            | _ -> None
        | Val b :: rest ->
            match stack with
            | Plus :: Val a :: restStack -> eval' (Val (a + b) :: restStack) rest 
            | _ -> eval' (Val b :: stack) rest
        | Plus :: rest -> eval' (Plus :: stack) rest
        | Times :: rest -> 
            match stack with
            | Val x :: Times :: Val y :: restStack -> eval' (Times :: Val (x * y) :: restStack) rest
            | _ -> eval' (Times :: stack) rest
        | Open :: rest -> eval' (Open :: stack) rest
        | Close :: rest ->
            match stack with
            | Val a :: Open :: restStack -> eval' restStack (Val a :: rest)
            | Val b :: Times :: Val a :: Open :: restStack -> eval' restStack (Val (a * b) :: rest)
            | _ -> None
    eval' [] expr

let answer2 =
    input
    |> List.map (tokenize >> eval2)
    |> List.reduce (Option.map2 (+))
open System
open System.IO
open System.Text.RegularExpressions

let inputs =
    File.ReadLines(@"day07/input.txt")
    |> Seq.toList

// let inputs = [
//     "$ cd /"
//     "$ ls"
//     "dir a"
//     "14848514 b.txt"
//     "8504156 c.dat"
//     "dir d"
//     "$ cd a"
//     "$ ls"
//     "dir e"
//     "29116 f"
//     "2557 g"
//     "62596 h.lst"
//     "$ cd e"
//     "$ ls"
//     "584 i"
//     "$ cd .."
//     "$ cd .."
//     "$ cd d"
//     "$ ls"
//     "4060174 j"
//     "8033020 d.log"
//     "5626152 d.ext"
//     "7214296 k"
// ]

type Command = Ls | CdUp | Cd of string
type Node = Dir of string | File of int64 * string
type Line = Cmd of Command | Listing of Node

let (|Integer|_|) (str: string) =
   let mutable intvalue = 0L
   if System.Int64.TryParse(str, &intvalue) then Some(intvalue)
   else None

let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None

let parseLine (line: string): Line =
    match line with
    | ParseRegex "(\d+) (.+)$" [Integer size; name] -> Listing (File (size, name))
    | ParseRegex "dir (.+)$" [name] -> Listing (Dir name)
    | ParseRegex "\$ ls" [] -> Cmd Ls
    | ParseRegex "\$ cd ..$" [] -> Cmd CdUp
    | ParseRegex "\$ cd (.+)$" [dst] -> Cmd (Cd dst)
    | _ -> failwith "unrecognized line"

let lines =
    inputs |> List.tail |> List.map parseLine

type FsTree =
    { Size: int64; Name: string; Children: FsTree list }

    static member Empty (s: string) = {
        Name = s
        Size = 0
        Children = []
    }
    
let rec totalSize (tree: FsTree): int64 =
    match tree.Children with
    | [] -> tree.Size
    | _ -> List.sumBy totalSize tree.Children
    
let rec addNode' (tree: FsTree) (path': string list) (newNode: FsTree): FsTree =
    match path' with
    | [] -> { tree with Children = newNode :: tree.Children }
    | cwd :: restPath ->
        let addChild c =
            if c.Name = cwd
            then addNode' c restPath newNode
            else c
        let updated = tree.Children |> List.map addChild
        { tree with Children = updated }

let addNode (tree: FsTree) (path': string list) (newNode: FsTree): FsTree =
    let path = List.rev path'
    addNode' tree path newNode
    
let rec mkTree (acc: FsTree) (path: string list) (lines: Line list): FsTree =
    match lines with
    | [] -> acc
    | line :: rest ->
        match line with
        | Cmd (Cd d) -> mkTree acc (d :: path) rest
        | Cmd (CdUp) -> mkTree acc (List.tail path) rest
        | Cmd Ls -> mkTree acc path rest
        | Listing (Dir d) -> mkTree (addNode acc path (FsTree.Empty d)) path rest
        | Listing (File (size, name)) -> mkTree (addNode acc path { Size = size; Name = name; Children = [] }) path rest
    
let rec dirs' (tree: FsTree): (bool * string * int64) list =
    match tree.Children with
    | [] -> [(false, tree.Name, tree.Size)]
    | cs -> 
        let listed = cs |> List.collect (fun c -> dirs' c)
        let size = listed |> List.sumBy (fun (isDir,_,x) -> if isDir then 0L else x)
        (true, tree.Name, size) :: listed

let dirs (tree: FsTree): (string * int64) list =
    tree |> dirs' |> List.filter (fun (x,_,_) -> x) |> List.map (fun (_,x,y) -> (x,y))

let tree = mkTree (FsTree.Empty "/") [] lines

let answer1 =
    tree
    |> dirs
    |> List.filter (snd >> (fun x -> x <= 100_000L)) |> List.sumBy snd

printfn "Part 1: %A" answer1

let occupied = tree |> dirs |> List.head |> snd
let answer2 = tree |> dirs |> List.map snd |> List.filter (fun x -> 40_000_000L + x > occupied) |> List.min

printfn "Part 2: %A" answer2
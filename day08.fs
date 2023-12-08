module Day08

open System.IO

type Move =
    | Left
    | Right

type Node =
    { Name: string
      Left: string
      Right: string }

type State = string * int

let parseMoves (line: string) =
    line
    |> Seq.map (function
        | 'L' -> Left
        | 'R' -> Right
        | _ -> failwith "Unsupported character")
    |> Seq.toArray

let parseNode (line: string) =
    let name = line[..2]
    let left = line[7..9]
    let right = line[12..14]

    { Name = name
      Left = left
      Right = right }

let parseNodes lines =
    lines |> Array.map parseNode |> Array.map (fun n -> (n.Name, n)) |> Map.ofArray

let rec traverse (nodes: Map<string, Node>) (moves: Move array) (states: State list) =
    let newStates =
        states
        |> List.map (function
            | (n, m) when n.EndsWith("Z") -> (n, m)
            | (n, m) ->
                let node = nodes[n]

                match moves.[int m % Array.length moves] with
                | Left -> (node.Left, m + 1)
                | Right -> (node.Right, m + 1))

    if newStates |> List.forall (fun (n, _) -> n.EndsWith("Z")) then
        newStates |> List.map snd
    else
        traverse nodes moves newStates

let rec gcd a b = if b = 0UL then a else gcd b (a % b)

let lcm a b = a * (b / gcd a b)

let part1 nodes moves =
    traverse nodes moves [ "AAA", 0 ] |> List.head

let part2 (nodes: Map<string, Node>) moves =
    nodes
    |> Map.keys
    |> Seq.filter (fun k -> k.EndsWith("A"))
    |> Seq.map (fun k -> (k, 0))
    |> Seq.toList
    |> traverse nodes moves
    |> List.map uint64
    |> List.fold lcm 1UL

let run =
    printfn "== Day08 =="

    let lines = File.ReadAllLines("inputs/day08.txt")
    let moves = parseMoves lines[0]
    let nodes = parseNodes lines[2..]

    printfn "Part 1: %d" (part1 nodes moves)
    printfn "Part 2: %d" (part2 nodes moves)
    printfn ""

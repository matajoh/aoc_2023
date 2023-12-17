module Day17

open System.IO

open Algorithms

type Direction =
    | Start
    | North
    | East
    | West
    | South

type Step =
    { Move: Direction
      Count: int
      Row: int
      Column: int }

type City =
    { Rows: int
      Columns: int
      Blocks: int[,] }

let parseLine line =
    line |> Seq.map (fun c -> int (c - '0')) |> Seq.toList

let parseCity lines =
    let blocks = lines |> Seq.map parseLine |> array2D

    { Rows = Array2D.length1 blocks
      Columns = Array2D.length2 blocks
      Blocks = blocks }

let distance city _ s = Array2D.get city.Blocks s.Row s.Column

let heuristic city s =
    (city.Rows - s.Row - 1) + (city.Columns - s.Column - 1)

let goal city s =
    s.Row = city.Rows - 1 && s.Column = city.Columns - 1

let inBounds city s =
    not (s.Row < 0 || s.Row >= city.Rows || s.Column < 0 || s.Column >= city.Columns)

let north c s =
    { s with
        Move = North
        Row = s.Row - 1
        Count = c }

let east c s =
    { s with
        Move = East
        Column = s.Column + 1
        Count = c }

let west c s =
    { s with
        Move = West
        Column = s.Column - 1
        Count = c }

let south c s =
    { s with
        Move = South
        Row = s.Row + 1
        Count = c }

let neighbors city s =
    match s.Move, s.Count with
    | Start, _ -> [ north 1; east 1; west 1; south 1 ]
    | North, c when c = 3 -> [ west 1; east 1 ]
    | North, c -> [ north (c + 1); east 1; west 1 ]
    | East, c when c = 3 -> [ north 1; south 1 ]
    | East, c -> [ east (c + 1); north 1; south 1 ]
    | West, c when c = 3 -> [ north 1; south 1 ]
    | West, c -> [ west (c + 1); north 1; south 1 ]
    | South, c when c = 3 -> [ west 1; east 1 ]
    | South, c -> [ south (c + 1); west 1; east 1 ]
    |> Seq.map (fun m -> m s)
    |> Seq.filter (inBounds city)

let ultraNeighbors city s =
    match s.Move, s.Count with
    | Start, _ -> [ north 1; east 1; west 1; south 1 ]
    | North, c when c = 10 -> [ east 1; west 1 ]
    | North, c when c < 4 -> [ north (c + 1) ]
    | North, c -> [ north (c + 1); east 1; west 1 ]
    | East, c when c = 10 -> [ north 1; south 1 ]
    | East, c when c < 4 -> [ east (c + 1) ]
    | East, c -> [ east (c + 1); north 1; south 1 ]
    | West, c when c = 10 -> [ north 1; south 1 ]
    | West, c when c < 4 -> [ west (c + 1) ]
    | West, c -> [ west (c + 1); north 1; south 1 ]
    | South, c when c = 10 -> [ east 1; west 1 ]
    | South, c when c < 4 -> [ south (c + 1) ]
    | South, c -> [ south (c + 1); east 1; west 1 ]
    |> Seq.map (fun m -> m s)
    |> Seq.filter (inBounds city)

let ultraGoal city s =
    s.Row = city.Rows - 1 && s.Column = city.Columns - 1 && s.Count >= 4

let part1 city =
    let astar = AStar<Step>(distance city, heuristic city, neighbors city, goal city)

    let start =
        { Move = Start
          Count = 0
          Row = 0
          Column = 0 }

    let path = astar.FindMinPath start
    path |> List.map (fun s -> Array2D.get city.Blocks s.Row s.Column) |> List.sum

let part2 city =
    let astar =
        AStar<Step>(distance city, heuristic city, ultraNeighbors city, ultraGoal city)

    let start =
        { Move = Start
          Count = 0
          Row = 0
          Column = 0 }

    let path = astar.FindMinPath start
    path |> List.map (fun s -> Array2D.get city.Blocks s.Row s.Column) |> List.sum

let run =
    printfn "== Day 17 =="

    let city = File.ReadLines("inputs/day17.txt") |> parseCity

    printfn "Part 1: %d" (part1 city)
    printfn "Part 2: %d" (part2 city)
    printfn ""

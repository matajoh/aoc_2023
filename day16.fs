module Day16

open System
open System.IO

type Tile =
    | Empty
    | RMirror
    | LMirror
    | LRSplitter
    | UDSplitter

[<Flags>]
type Direction =
    | Empty = 0y
    | Up = 1y
    | Down = 2y
    | Right = 4y
    | Left = 8y

let parseLine line =
    line
    |> Seq.map (function
        | '.' -> Empty
        | '/' -> RMirror
        | '\\' -> LMirror
        | '-' -> LRSplitter
        | '|' -> UDSplitter
        | _ -> failwith "Invalid character")

let parseTiles lines = lines |> Seq.map parseLine |> array2D

let moveBeam tiles (d, r, c) =
    match d, Array2D.get tiles (int r) (int c) with
    | Direction.Up, RMirror -> [ Direction.Right, r, c + 1y ]
    | Direction.Up, LMirror -> [ Direction.Left, r, c - 1y ]
    | Direction.Up, LRSplitter -> [ Direction.Left, r, c - 1y; Direction.Right, r, c + 1y ]
    | Direction.Up, _ -> [ Direction.Up, r - 1y, c ]
    | Direction.Right, RMirror -> [ Direction.Up, r - 1y, c ]
    | Direction.Right, LMirror -> [ Direction.Down, r + 1y, c ]
    | Direction.Right, UDSplitter -> [ Direction.Up, r - 1y, c; Direction.Down, r + 1y, c ]
    | Direction.Right, _ -> [ Direction.Right, r, c + 1y ]
    | Direction.Left, RMirror -> [ Direction.Down, r + 1y, c ]
    | Direction.Left, LMirror -> [ Direction.Up, r - 1y, c ]
    | Direction.Left, UDSplitter -> [ Direction.Up, r - 1y, c; Direction.Down, r + 1y, c ]
    | Direction.Left, _ -> [ Direction.Left, r, c - 1y ]
    | Direction.Down, RMirror -> [ Direction.Left, r, c - 1y ]
    | Direction.Down, LMirror -> [ Direction.Right, r, c + 1y ]
    | Direction.Down, LRSplitter -> [ Direction.Left, r, c - 1y; Direction.Right, r, c + 1y ]
    | Direction.Down, _ -> [ Direction.Down, r + 1y, c ]
    | _ -> failwith "Invalid direction"

let inBounds tiles (_, r, c) =
    let rows = sbyte (Array2D.length1 tiles)
    let cols = sbyte (Array2D.length2 tiles)
    not (r < 0y || r = rows || c < 0y || c = cols)

let energize energized (d: Direction, rr, cc) =
    let r = int rr
    let c = int cc
    let energy = Array2D.get energized r c

    if energy &&& d = d then
        false
    else
        Array2D.set energized r c (energy ||| d)
        true

let rec shineBeams energized tiles beams =
    match beams with
    | [] -> energized
    | beam :: tail ->
        let next =
            if energize energized beam then
                beam |> moveBeam tiles |> List.filter (inBounds tiles)
            else
                []

        shineBeams energized tiles (next @ tail)

let countEnergized tiles start =
    let rows = Array2D.length1 tiles
    let cols = Array2D.length2 tiles

    let energized =
        shineBeams (Array2D.create rows cols Direction.Empty) tiles [ start ]

    seq {
        for r in 0 .. (rows - 1) do
            for c in 0 .. (cols - 1) do
                if Array2D.get energized r c = Direction.Empty then
                    yield 0
                else
                    yield 1
    }
    |> Seq.sum

let edgeStarts tiles =
    let rows = sbyte (Array2D.length1 tiles)
    let cols = sbyte (Array2D.length2 tiles)

    seq {
        yield!
            seq { 0y .. rows - 1y }
            |> Seq.collect (fun r -> [ (Direction.Right, r, 0y); (Direction.Left, r, cols - 1y) ])

        yield!
            seq { 0y .. cols - 1y }
            |> Seq.collect (fun c -> [ (Direction.Down, 0y, c); (Direction.Up, rows - 1y, c) ])
    }

let part1 tiles =
    countEnergized tiles (Direction.Right, 0y, 0y)

let part2 tiles =
    tiles |> edgeStarts |> Seq.map (countEnergized tiles) |> Seq.max

let run =
    printfn "== Day 16 =="

    let tiles = File.ReadLines("inputs/day16.txt") |> parseTiles

    printfn "Part 1: %A" (part1 tiles)
    printfn "Part 2: %d" (part2 tiles)
    printfn ""

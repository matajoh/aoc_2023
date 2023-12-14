module Day14

open System.IO

type Tile =
    | Empty
    | Round
    | Cube

type Platform =
    { Rows: int
      Columns: int
      Tiles: Tile[,]
      Buffer: Tile[,] }

    static member swap p =
        { p with
            Tiles = p.Buffer
            Buffer = p.Tiles }

    static member get p = Array2D.get p.Tiles
    static member set p = Array2D.set p.Buffer

    static member totalLoad p =
        seq {
            for r in 0 .. p.Rows - 1 do
                for c in 0 .. p.Columns - 1 do
                    match Platform.get p r c with
                    | Round -> yield p.Rows - r
                    | _ -> yield 0
        }
        |> Seq.sum

let parseLine (line: string) =
    line
    |> Seq.map (function
        | '.' -> Empty
        | 'O' -> Round
        | '#' -> Cube
        | _ -> failwith "Unexpected char")
    |> Seq.toList

let parsePlatform lines =
    let rows = List.length lines
    let columns = String.length lines[0]
    let tiles = lines |> List.map parseLine |> array2D

    { Rows = rows
      Columns = columns
      Tiles = tiles
      Buffer = Array2D.copy tiles }

let tiltNorth p =
    for c in 0 .. p.Columns - 1 do
        let mutable r0 = 0

        for r in 0 .. p.Rows - 1 do
            match Platform.get p r c with
            | Round ->
                Platform.set p r c Empty
                Platform.set p r0 c Round
                r0 <- r0 + 1
            | Cube -> r0 <- r + 1
            | Empty -> Platform.set p r c Empty

    Platform.swap p

let tiltWest p =
    for r in 0 .. p.Rows - 1 do
        let mutable c0 = 0

        for c in 0 .. p.Columns - 1 do
            match Platform.get p r c with
            | Round ->
                Platform.set p r c Empty
                Platform.set p r c0 Round
                c0 <- c0 + 1
            | Cube -> c0 <- c + 1
            | Empty -> Platform.set p r c Empty

    Platform.swap p

let tiltSouth p =
    for c in 0 .. p.Columns - 1 do
        let mutable r0 = p.Rows - 1

        for r in p.Rows - 1 .. -1 .. 0 do
            match Platform.get p r c with
            | Round ->
                Platform.set p r c Empty
                Platform.set p r0 c Round
                r0 <- r0 - 1
            | Cube -> r0 <- r - 1
            | Empty -> Platform.set p r c Empty

    Platform.swap p

let tiltEast p =
    for r in 0 .. p.Rows - 1 do
        let mutable c0 = p.Columns - 1

        for c in p.Columns - 1 .. -1 .. 0 do
            match Platform.get p r c with
            | Round ->
                Platform.set p r c Empty
                Platform.set p r c0 Round
                c0 <- c0 - 1
            | Cube -> c0 <- c - 1
            | Empty -> Platform.set p r c Empty

    Platform.swap p

let spin p =
    p |> tiltNorth |> tiltWest |> tiltSouth |> tiltEast

let rec sample n platform =
    match n with
    | 0 -> []
    | _ -> (Platform.totalLoad platform) :: (sample (n - 1) (spin platform))

let isPattern length samples =
    let start = List.length samples - 2 * length

    List.zip samples[start .. start + length - 1] samples[start + length ..]
    |> List.exists (fun (a, b) -> a <> b)
    |> (not)

let rec findPattern length samples =
    match length with
    | 0 -> failwith "Pattern not found"
    | _ when isPattern length samples -> samples[List.length samples - length ..]
    | _ -> findPattern (length - 1) samples

let part1 platform =
    platform |> tiltNorth |> Platform.totalLoad

let part2 platform =
    let numSamples = 200
    let pattern = sample numSamples platform |> findPattern 50
    pattern[(1000000000 - numSamples) % List.length pattern]

let run =
    printfn "== Day 14 =="

    let platform = File.ReadLines("inputs/day14.txt") |> Seq.toList |> parsePlatform

    printfn "Part 1: %d" (part1 platform)
    printfn "Part 2: %A" (part2 platform)
    printfn ""

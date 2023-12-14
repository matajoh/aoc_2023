module Day10

open System.IO

type Direction =
    | North
    | East
    | South
    | West

type Tile =
    | Ground
    | Start
    | Pipe of Direction * Direction

type Point =
    { X: int
      Y: int }

    static member neighbor point dir =
        match dir with
        | North -> { point with Y = point.Y - 1 }
        | East -> { point with X = point.X + 1 }
        | South -> { point with Y = point.Y + 1 }
        | West -> { point with X = point.X - 1 }

type Tiles =
    { Width: int
      Height: int
      Items: Tile list list
      Start: Point }

    static member inBounds tiles point =
        not (point.Y < 0 || point.Y >= tiles.Height || point.X < 0 || point.X >= tiles.Width)

    static member get tiles point = tiles.Items[point.Y][point.X]

    member this.Item
        with get (point) = Tiles.get this point

    static member tryGet tiles point =
        if Tiles.inBounds tiles point then
            Some(tiles[point])
        else
            None

    static member connects tiles point dir =
        let n = Point.neighbor point dir

        match dir, Tiles.tryGet tiles n with
        | North, Some(Pipe(South, _)) -> true
        | North, Some(Pipe(_, South)) -> true
        | South, Some(Pipe(North, _)) -> true
        | East, Some(Pipe(_, West)) -> true
        | West, Some(Pipe(_, East)) -> true
        | West, Some(Pipe(East, _)) -> true
        | _ -> false

    static member toString tiles loop =
        [ for y in [ 0 .. tiles.Height - 1 ] do
              [| for x in [ 0 .. tiles.Width - 1 ] do
                     let p = { X = x; Y = y }

                     match tiles[p], Set.contains p loop with
                     | Pipe(North, South), true -> '│'
                     | Pipe(East, West), true -> '━'
                     | Pipe(North, East), true -> '┕'
                     | Pipe(North, West), true -> '┙'
                     | Pipe(South, West), true -> '┑'
                     | Pipe(South, East), true -> '┍'
                     | Start, true -> 'S'
                     | _ -> '.' |]
              |> System.String ]
        |> (String.concat "\n")


let parsePipe c =
    match c with
    | '|' -> Pipe(North, South)
    | '-' -> Pipe(East, West)
    | 'L' -> Pipe(North, East)
    | 'J' -> Pipe(North, West)
    | '7' -> Pipe(South, West)
    | 'F' -> Pipe(South, East)
    | '.' -> Ground
    | 'S' -> Start
    | _ -> failwith "Invalid pipe"

let parseLine (line: string) = line |> Seq.map parsePipe |> List.ofSeq

let parseLines (lines: string seq) =
    lines |> Seq.map parseLine |> List.ofSeq

let rec findStart (tiles: Tile list list) p =
    match p.X + 1, p.Y + 1, tiles[p.Y][p.X] with
    | _, _, Start -> p
    | x, y, _ when x = List.length tiles[0] && y = List.length tiles -> failwith "Unable to located start node"
    | x, y, _ when x = List.length tiles[0] -> findStart tiles { X = 0; Y = y }
    | x, _, _ -> findStart tiles { p with X = x }

let parseTiles lines =
    let tiles = parseLines lines
    let start = findStart tiles { X = 0; Y = 0 }

    { Height = List.length tiles
      Width = List.length tiles[0]
      Items = tiles
      Start = start }

let neighbors tiles point =
    match Tiles.get tiles point with
    | Pipe(a, b) -> Point.neighbor point a, Point.neighbor point b
    | Start ->
        let neighbors =
            [ North; East; South; West ]
            |> List.filter (Tiles.connects tiles point)
            |> List.map (Point.neighbor point)

        match neighbors with
        | [ a; b ] -> a, b
        | _ -> failwith "Invalid starting point"

    | _ -> failwith "Unable to determine neighbors"

let findLoop tiles =
    let current = fst (neighbors tiles tiles.Start)

    let rec f tiles start prev current acc =
        if current = start then
            List.rev (start :: acc)
        else
            let (a, b) = neighbors tiles current

            if a = prev then
                f tiles start current b (current :: acc)
            else
                f tiles start current a (current :: acc)

    f tiles tiles.Start tiles.Start current []

let part1 loop =
    let length = Set.count loop
    length / 2 + length % 2

let rec countIntersects tiles loop cache point =
    if Tiles.inBounds tiles point then
        match Array2D.get cache point.X point.Y with
        | Some(x) -> x
        | None ->
            let x =
                match tiles[point], Set.contains point loop with
                | Pipe(North, _), true -> 1 + (countIntersects tiles loop cache { point with X = point.X + 1 })
                | _ -> countIntersects tiles loop cache { point with X = point.X + 1 }

            cache[point.X, point.Y] <- Some(x)
            x
    else
        0

let isOdd x = x % 2 = 1

let part2 tiles loop =
    let cache = Array2D.create tiles.Width tiles.Height None

    [ for x in [ 0 .. tiles.Width ] do
          for y in [ 0 .. tiles.Height ] do
              yield { X = x; Y = y } ]
    |> List.filter (fun p -> not (Set.contains p loop))
    |> List.map (countIntersects tiles loop cache)
    |> List.filter isOdd
    |> List.length

let run =
    printfn "== Day 10 =="

    let tiles = File.ReadLines("inputs/day10.txt") |> parseTiles
    let loop = findLoop tiles |> set

    printfn "Part 1: %d" (part1 loop)
    printfn "Part 2: %d" (part2 tiles loop)
    printfn ""

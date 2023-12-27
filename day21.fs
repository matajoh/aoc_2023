module Day21

open System.IO
open Math

type Point =
    { X: int
      Y: int }

    static member neighbors (p : Point) =
        seq {
            { p with X = p.X - 1 }
            { p with X = p.X + 1 }
            { p with Y = p.Y - 1 }
            { p with Y = p.Y + 1 }
        }

type Tile =
    | Start of Point
    | Rock of Point

type Garden =
    { Width: int
      Height: int
      Rocks: Set<Point>
      Start: Point }

    static member create w h =
        { Width = w
          Height = h
          Rocks = Set.empty
          Start = { X = 0; Y = 0 } }

    static member addTile g t =
        match t with
        | Rock(p) -> { g with Rocks = Set.add p g.Rocks }
        | Start(p) -> { g with Start = p }

    static member canMoveTo g p =
        Set.contains { X = modulo p.X g.Width; Y = modulo p.Y g.Height } g.Rocks |> not

let parseLine y line =
    line
    |> Seq.mapi (fun x c ->
        match c with
        | '#' -> Some(Rock({ X = x; Y = y }))
        | 'S' -> Some(Start({ X = x; Y = y }))
        | _ -> None)
    |> Seq.choose id

let parseGarden lines =
    let height = Seq.length lines
    let width = String.length (Seq.head lines)

    lines
    |> Seq.mapi parseLine
    |> Seq.concat
    |> Seq.fold Garden.addTile (Garden.create width height)

let rec step counts garden i n points =
    if i = n then
        List.rev counts
    else
        let newPoints =
            points
            |> Seq.collect Point.neighbors
            |> Seq.filter (Garden.canMoveTo garden)
            |> set

        step (Set.count newPoints :: counts) garden (i + 1) n newPoints

let part1 g =
    step [] g 0 64 (Set.singleton g.Start) |> List.last

let part2 g =
    let target = 26501365
    let rem = target % g.Width
    let n = rem + 2 * g.Width + 1
    let counts = step [ 1 ] g 0 n (Set.singleton g.Start)

    let r0, r1, r2 = counts[rem], counts[rem + g.Width], counts[rem + 2 * g.Width]
    let c = r0
    let a = (r2 - r0 - 2 * (r1 - r0)) / 2
    let b = r1 - r0 - a
    let x = (target - rem) / g.Width
    a, b, c, x

let run =
    printfn "== Day 21 =="

    let g = File.ReadLines("inputs/day21.txt") |> parseGarden

    // uncomment to get the values
    //let a, b, c, x = part2 g
    //printfn "a=%d b=%d c=%d x=%d" a b c x
    let a, b, c, x = 15135, 15251, 3867, 202300

    printfn "Part 1: %d" (part1 g)
    printfn "Part 2: %d" (quadratic a b c x)
    printfn ""

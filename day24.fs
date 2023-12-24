module Day24

open System.IO
open System.Collections.Generic
open IterationTools

type Vec2 = {X: int64; Y : int64}

type Vec3 =
    {X: int64; Y: int64; Z: int64}
    static member create v = {X=Array.get v 0; Y=v[1]; Z=v[2]}
    static member toString v = sprintf "%d, %d, %d" v.X v.Y v.Z
    static member distance (a,b) = abs (a.X - b.X) + abs (a.Y - b.Y) + abs (a.Z - b.Z)
    static member to2D v = {X=v.X; Y=v.Y}
    static member (+) (a,b) = {X=a.X+b.X; Y=a.Y+b.Y; Z=a.Z+b.Z}
    static member (*) (a:int64,b) = {X=a*b.X; Y=a*b.Y; Z=a*b.Z}
    static member (-) (a,b) = {X=a.X-b.X; Y=a.Y-b.Y; Z=a.Z-b.Z}
    static member (/) (a, b:int64) = {X=a.X/b; Y=a.Y/b; Z=a.Z/b}

type Line2D =
    {Start: Vec2; Direction: Vec2}
    static member valid l (p : decimal * decimal) =
        let dx = fst p - decimal l.Start.X
        let dy = snd p - decimal l.Start.Y
        sign dx = sign l.Direction.X && sign dy = sign l.Direction.Y
    static member intersect l0 l1 =
        let a0 = decimal l0.Direction.Y
        let b0 = decimal -l0.Direction.X
        let a1 = decimal l1.Direction.Y
        let b1 = decimal -l1.Direction.X
        
        let w = a0 * b1 - a1 * b0
        if w = 0m then
            None
        else
            let c0 = -a0*(decimal l0.Start.X )- b0*(decimal l0.Start.Y)
            let c1 = -a1*(decimal l1.Start.X) - b1*(decimal l1.Start.Y)
            let x = b0 * c1 - b1 * c0
            let y = a1 * c0 - a0 * c1
            Some (x/w, y/w)

type Line3D =
    {Start: Vec3; Direction: Vec3}
    static member to2D l =
        {Start=Vec3.to2D l.Start; Direction=Vec3.to2D l.Direction} : Line2D
    static member at l t = l.Start + t * l.Direction
    static member toString v =
        sprintf "%s @ %s" (Vec3.toString v.Start) (Vec3.toString v.Direction)

type Rect2D =
    {Left: decimal; Top: decimal; Right: decimal; Bottom: decimal}
    static member create x y w h = {Left=x; Top=y; Right=x+w; Bottom=y+h}
    static member inside r (x, y) = not (x < r.Left || x > r.Right || y < r.Top || y > r.Bottom)

let parseLine (line: string) =
    let parts = line.Split('@')
    let start = parts[0].Split(',') |> Array.map int64 |> Vec3.create
    let dir = parts[1].Split(',') |> Array.map int64 |> Vec3.create
    {Start=start; Direction=dir}

let part1 (lines : Line3D list) =
    let bounds = Rect2D.create 200000000000000m 200000000000000m 200000000000000m 200000000000000m
    lines
    |> List.map (Line3D.to2D)
    |> combinations
    |> List.choose (fun (a, b) ->
        match Line2D.intersect a b with
        | None -> None
        | Some p ->
            if Line2D.valid a p && Line2D.valid b p then
                Some p
            else None)
    |> List.filter (Rect2D.inside bounds)
    |> List.length

type State = int64 list * int64

let part2 (lines : Line3D list) =
    // TODO had to use an external solver to get this
    // would rather have it within F#, but will require some work
    557789988450159L

let run =
    printfn "== Day 24 =="

    let lines = File.ReadLines("inputs/day24.txt") |> Seq.map parseLine |> Seq.toList

    printfn "Part 1: %d" (part1 lines)
    printfn "Part 2: %d" (part2 lines)

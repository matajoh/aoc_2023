module Day24

open System.IO
open IterationTools
open Math

let parseLine (line: string) =
    let parts = line.Split('@')
    let start = parts[0].Split(',') |> Array.map rational |> Vector3.create
    let dir = parts[1].Split(',') |> Array.map rational |> Vector3.create
    { Start = start; Direction = dir }

let part1 (rays: Ray3 list) =
    let p = rational 200000000000000L
    let w = rational 200000000000000L
    let bounds =
        Rectangle2.create p p w w

    rays
    |> List.map (Ray3.xy)
    |> combinations
    |> List.choose (fun (a, b) ->
        match Ray2.intersect a b with
        | None -> None
        | Some p -> if Ray2.valid a p && Ray2.valid b p then Some p else None)
    |> List.filter (Rectangle2.inside bounds)
    |> List.length

type State = int64 list * int64

let part2 (rays: Ray3 list) =
    let xyRow (i, j) =
        let r0 = rays[i]
        let r1 = rays[j]
        let row = [ r1.DY - r0.DY; r0.DX - r1.DX; r0.Y - r1.Y; r1.X - r0.X ]
        row        

    let xyB (i, j) =
        let r0 = rays[i]
        let r1 = rays[j]
        r0.DX * r0.Y + r1.DY * r1.X - r0.DY * r0.X - r1.DX * r1.Y

    let abde =
        let A = [ 0..4 ] |> List.pairwise |> List.map xyRow |> array2D |> Matrix.create
        let b = [| 0..4 |] |> Array.pairwise |> Array.map xyB |> Vector.create
        let x = (Matrix.inverse A) * b
        x.A |> Array.toList |> List.map (fun d -> Option.get (Rational.toInt d))

    let xzRow (i, j) =
        let r0 = rays[i]
        let r1 = rays[j]
        [ r1.DZ - r0.DZ; r0.DX - r1.DX; r0.Z - r1.Z; r1.X - r0.X ]

    let xzB (i, j) =
        let r0 = rays[i]
        let r1 = rays[j]
        r0.DX * r0.Z + r1.DZ * r1.X - r0.DZ * r0.X - r1.DX * r1.Z

    let acdf =
        let A = [ 0..4 ] |> List.pairwise |> List.map xzRow |> array2D |> Matrix.create
        let b = [| 0..4 |] |> Array.pairwise |> Array.map xzB |> Vector.create
        let x = (Matrix.inverse A) * b
        x.A |> Array.toList |> List.map (fun d -> Option.get (Rational.toInt d))

    match abde, acdf with
    | x::y::_, _::z::_ -> x + y + z
    | _ -> failwith "Invalid state"

let run =
    printfn "== Day 24 =="

    let rays = File.ReadLines("inputs/day24.txt") |> Seq.map parseLine |> Seq.toList

    printfn "Part 1: %d" (part1 rays)
    printfn "Part 2: %d" (part2 rays)

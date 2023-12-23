module Day22

open System.Collections.Generic
open System.IO

type Point =
    { X: int
      Y: int
      Z: int }

    static member drop d p = { p with Z = p.Z - d }

type Brick =
    { ID: int
      Start: Point
      End: Point }

    static member bottom b = b.Start.Z
    static member top b = b.End.Z + 1
    static member left b = b.Start.X
    static member right b = b.End.X + 1
    static member front b = b.Start.Y
    static member back b = b.End.Y + 1

    static member intersects b0 b1 =
        let x0 = max (Brick.left b0) (Brick.left b1)
        let x1 = min (Brick.right b0) (Brick.right b1)
        let y0 = max (Brick.front b0) (Brick.front b1)
        let y1 = min (Brick.back b0) (Brick.back b1)

        if x0 >= x1 then false
        elif y0 >= y1 then false
        else true

    static member drop b =
        { b with
            Start = Point.drop 1 b.Start
            End = Point.drop 1 b.End }

let remove i s =
    match s with
    | Some is -> Some(Set.remove i is)
    | None -> failwith "error"

let add i s =
    match s with
    | Some is -> Some(Set.add i is)
    | None -> Some(Set.singleton i)

let brickMap f bricks =
    bricks
    |> List.map (fun b -> f b, b)
    |> List.groupBy fst
    |> List.map (fun (i, bs) -> i, bs |> List.map (fun (_, b) -> b.ID) |> Set)
    |> Map

let moveBrick map i j b =
    map |> Map.change i (remove b.ID) |> Map.change j (add b.ID)

type Stack =
    { Bricks: Brick list
      Tops: Map<int, Set<int>>
      Bottoms: Map<int, Set<int>> }

    static member create bricks =
        { Bricks = bricks |> List.sortBy (fun b -> b.ID)
          Tops = bricks |> brickMap Brick.top
          Bottoms = bricks |> brickMap Brick.bottom }

    static member supports s i =
        let b = s.Bricks[i]

        match Map.tryFind (Brick.bottom b) s.Tops with
        | None -> Set.empty
        | Some bs -> bs |> Set.filter (fun j -> Brick.intersects b s.Bricks[j])

    static member notSupported s i = Stack.supports s i |> Set.isEmpty

    static member drop s i =
        let rec f b =
            if Brick.bottom b = 0 then
                b
            else
                match Map.tryFind (Brick.bottom b) s.Tops with
                | Some bs when bs |> Set.exists (fun j -> Brick.intersects b s.Bricks[j]) -> b
                | _ -> f (Brick.drop b)

        let b = s.Bricks[i]
        let b' = f b

        { s with
            Bricks = List.updateAt i b' s.Bricks
            Tops = moveBrick s.Tops (Brick.top b) (Brick.top b') b
            Bottoms = moveBrick s.Bottoms (Brick.bottom b) (Brick.bottom b') b }

    static member remove s i =
        let rec f c s removed =
            match removed with
            | [] -> c - 1
            | i :: tail ->
                let b = s.Bricks[i]
                let tops = s.Tops |> Map.change (Brick.top b) (remove b.ID)
                let bottoms = s.Bottoms |> Map.change (Brick.bottom b) (remove b.ID)

                let s' =
                    { s with
                        Tops = tops
                        Bottoms = bottoms }

                let bs = s.Bottoms[Brick.top b] |> Seq.filter (Stack.notSupported s') |> Seq.toList
                f (c + 1) s' (bs @ tail)

        f 0 s [ i ]

let comparePoints p0 p1 =
    let z = compare p0.Z p1.Z

    if z <> 0 then
        z
    else
        let x = compare p0.X p1.X
        if x <> 0 then x else compare p0.Y p1.Y

let compareBricks b0 b1 =
    let s = comparePoints b0.Start b1.Start
    if s <> 0 then s else comparePoints b0.End b1.End

let parsePoint (line: string) =
    let parts = line.Split(',')

    { X = int parts[0]
      Y = int parts[1]
      Z = int parts[2] }

let parseBrick i (line: string) =
    let parts = line.Split('~')

    { ID = i
      Start = parsePoint parts[0]
      End = parsePoint parts[1] }

let parseBricks lines =
    lines |> Seq.mapi parseBrick |> Seq.toList

let rec drop s bricks =
    match bricks with
    | [] -> s
    | b :: bs -> drop (Stack.drop s b.ID) bs

let brickIDs bricks = bricks |> List.map (fun b -> b.ID)

let rec canRemove s r bricks =
    match bricks with
    | [] -> r
    | i :: tail ->
        let supports = Stack.supports s i

        if Set.count supports = 1 then
            canRemove s (r - supports) tail
        else
            canRemove s r tail

let part1 stack =
    let ids = brickIDs stack.Bricks
    canRemove stack (ids |> Set) ids |> Set.count

let part2 stack =
    stack.Bricks |> brickIDs |> List.map (Stack.remove stack) |> List.sum

let run =
    printfn "== Day 22 =="

    let bricks =
        File.ReadLines("inputs/day22.txt") |> parseBricks |> List.sortWith compareBricks

    let stack = drop (Stack.create bricks) bricks

    printfn "Part 1: %d" (part1 stack)
    printfn "Part 2: %A" (part2 stack)
    printfn ""

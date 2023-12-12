module Day11

open IterationTools

type Galaxy = { X: int; Y: int }


type Image =
    { Galaxies: Galaxy list
      Width: int
      Height: int
      EmptyRows: int Set
      EmptyCols: int Set }

let parseLine y line =
    line
    |> Seq.mapi (fun x c ->
        match c with
        | '#' -> Some({ X = x; Y = y })
        | _ -> None)
    |> Seq.toList

let parseLines lines =
    lines |> Seq.mapi parseLine |> Seq.toList

let isEmpty (galaxies: option<Galaxy> list list) indices =
    indices |> List.map (fun (x, y) -> galaxies[y][x]) |> List.forall Option.isNone

let emptyGalaxies galaxies indices =
    indices
    |> List.mapi (fun i x -> (i, x))
    |> List.filter (fun (_, x) -> x |> isEmpty galaxies)
    |> List.map fst
    |> Set.ofList

let parseImage lines =
    let galaxies = parseLines lines
    let height = List.length galaxies
    let width = List.length galaxies.Head
    let emptyRows = rowColumn width height |> emptyGalaxies galaxies
    let emptyCols = columnRow width height |> emptyGalaxies galaxies

    { Galaxies = galaxies |> List.concat |> List.choose id
      Width = width
      Height = height
      EmptyRows = emptyRows
      EmptyCols = emptyCols }

let rec expandedDistance empties expansion i0 i1 =
    if i0 > i1 then
        expandedDistance empties expansion i1 i0
    elif i0 = i1 then
        0UL
    elif Set.contains i0 empties then
        expansion + expandedDistance empties expansion (i0 + 1) i1
    else
        1UL + expandedDistance empties expansion (i0 + 1) i1

let distance image expansion g0 g1 =
    let dx = expandedDistance image.EmptyCols expansion g0.X g1.X
    let dy = expandedDistance image.EmptyRows expansion g0.Y g1.Y
    dx + dy

let sumLengths image expansion =
    combinations image.Galaxies
    |> Seq.map (fun (g0, g1) -> distance image expansion g0 g1)
    |> Seq.sum

let part1 image = sumLengths image 2UL

let part2 image = sumLengths image 1000000UL

let run =
    printfn "== Day11 =="

    let image = System.IO.File.ReadLines "inputs/day11.txt" |> parseImage

    printfn "Part 1: %i" (part1 image)
    printfn "Part 2: %i" (part2 image)
    printfn ""

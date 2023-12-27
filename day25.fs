module Day25

open System.IO

type NodeInfo =
    { ID: int
      Size: int }

    static member merge n0 n1 =
        { ID = n0.ID; Size = n0.Size + n1.Size }

type Graph =
    { Edges: List<int * int>
      Nodes: Map<int, NodeInfo> }

    static member count g = Map.count g.Nodes

    static member cut g = List.length g.Edges

    static member create pairs =
        let names = pairs |> List.collect (fun (a, b) -> [ a; b ]) |> List.distinct
        let lookup = names |> List.mapi (fun i s -> s, i) |> Map
        let nodes = names |> List.mapi (fun i s -> i, { ID = i; Size = 1 }) |> Map

        let edges =
            pairs
            |> List.map (fun (s0, s1) -> lookup[s0], lookup[s1])
            |> List.map (fun (a, b) -> if a > b then b, a else a, b)

        { Edges = edges; Nodes = nodes }

    static member mergeNodes (a, b) g =
        let edges =
            g.Edges
            |> List.choose (fun (x, y) ->
                if x = a then
                    if y = b then None else Some(a, y)
                elif x = b then
                    if a < y then Some(a, y) else Some(y, a)
                elif y = b then
                    if a < x then Some(a, x) else Some(x, a)
                else
                    Some(x, y))

        let nodes =
            g.Nodes
            |> Map.change a (fun _ -> Some(NodeInfo.merge g.Nodes[a] g.Nodes[b]))
            |> Map.remove b

        { g with Nodes = nodes; Edges = edges }

    static member removeRandomEdge (rng : System.Random) g =
        Graph.mergeNodes g.Edges[rng.Next(List.length g.Edges)] g

    static member nodes g = g.Nodes |> Map.toList |> List.map snd

    static member contract rng t g =
        let rec f c =
            if Graph.count c = t then
                c
            else
                c |> Graph.removeRandomEdge rng |> f

        f g

    // https://en.wikipedia.org/wiki/Karger%27s_algorithm
    static member kargerStein rng limit g =
        let rec f i limit cs =
            if i = limit then
                None
            else
                match cs with
                | [] -> failwith "Unable to find solution"
                | c :: tail ->
                    let n = Graph.count c

                    if n <= 6 then
                        let c' = Graph.contract rng 2 c
                        if Graph.cut c' = 3 then
                            printfn "%d" i
                            Some c'
                        else f (i + 1) limit tail
                    else
                        let t = int (System.Math.Ceiling(1.0 + (float n) / System.Math.Sqrt(2.0)))
                        let g1 = c |> Graph.contract rng t
                        let g2 = c |> Graph.contract rng t
                        f i limit (g1 :: g2 :: tail)

        f 0 limit [ g ]

let parseLine (line: string) =
    let parts = line.Split(":")
    let a = parts[0].Trim()
    parts[1].Trim().Split(" ") |> Array.toList |> List.map (fun b -> a, b)

let parseGraph lines =
    lines |> Seq.toList |> List.collect parseLine |> Graph.create

let rec findCut g =
    let rng = new System.Random()
    match Graph.kargerStein rng 8192 g with
    | Some g' -> g'
    | None ->
        printfn "trying another seed..."
        findCut g

let score g =
    g.Nodes |> Map.toList |> List.map (fun (_, n) -> n.Size) |> List.fold (*) 1

let part1 g =
    // Uncomment to find your solution. May take a (randomly) long time.
    //g |> findCut |> score
    601344

let run =
    printfn "== Day 25 =="

    let graph = File.ReadLines("inputs/day25.txt") |> parseGraph

    printfn "Part 1: %d" (part1 graph)
    printfn ""

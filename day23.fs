module Day23

open System.IO

type Direction =
    | North
    | East
    | South
    | West

type Tile =
    | Forest
    | Path
    | Slope of Direction

    static member isPath t =
        match t with
        | Path -> true
        | _ -> false

type Point = { Row: int; Column: int }

type Node =
    { ID: int
      Location: Point }

    static member create id p = { ID = id; Location = p }

type State =
    { Visited: Set<Point>
      Current: Point }

    static member add s x =
        { s with
            Visited = Set.add x s.Visited
            Current = x }

    static member singleton p =
        { Visited = Set.singleton p
          Current = p }

let neighbors trails state =
    let move p dir =
        match dir with
        | North -> { p with Row = p.Row - 1 }
        | East -> { p with Column = p.Column + 1 }
        | South -> { p with Row = p.Row + 1 }
        | West -> { p with Column = p.Column - 1 }

    let isForest trails p =
        match Array2D.get trails p.Row p.Column with
        | Forest -> true
        | _ -> false

    let outOfBounds trails p =
        let rows = Array2D.length1 trails
        let columns = Array2D.length2 trails
        p.Row < 0 || p.Row >= rows || p.Column < 0 || p.Column >= columns

    let steps trails state =
        let valid p =
            not (Set.contains p state.Visited || outOfBounds trails p || isForest trails p)

        match Array2D.get trails state.Current.Row state.Current.Column with
        | Path -> [ North; South; East; West ]
        | Slope(d) -> [ d ]
        | _ -> failwith "Invalid path"
        |> List.map (move state.Current)
        |> List.filter valid

    state |> (steps trails) |> List.map (State.add state)

type Graph =
    { Nodes: Node list
      Edges: (int * int) list array }

    static member create trails =
        let source = { Row = 0; Column = 1 }

        let sink =
            { Row = Array2D.length1 trails - 1
              Column = Array2D.length2 trails - 2 }

        let edgesOut edges i =
            edges
            |> List.filter (fun ((a, _), _) -> a = i)
            |> List.map (fun ((_, b), c) -> b, c)

        let rec f nodes edges frontier =
            match frontier with
            | [] ->
                let nodes' = Map.values nodes |> Seq.toList |> List.sortBy (fun n -> n.ID)
                let edges' = [| 0 .. List.length nodes' |] |> Array.map (edgesOut edges)
                { Nodes = nodes'; Edges = edges' }
            | (s, (a, n)) :: tail when s.Current = sink -> f nodes (((a, 1), n) :: edges) tail
            | (s, (a, n)) :: tail ->
                let next = neighbors trails s

                if List.length next = 1 then
                    let s' = List.head next
                    f nodes edges ((s', (a, n + 1)) :: tail)
                elif List.length next = 0 then
                    f nodes edges tail
                else
                    match Map.tryFind s.Current nodes with
                    | Some(b) -> f nodes (((a, b.ID), n + 1) :: edges) tail
                    | None ->
                        let b = Node.create (Map.count nodes) s.Current
                        let nodes' = Map.add s.Current b nodes
                        let edges' = ((a, b.ID), n + 1) :: edges
                        let frontier' = next |> List.map (fun s -> (s, (b.ID, 0))) |> List.append tail
                        f nodes' edges' frontier'

        let nodes = [ source, Node.create 0 source; sink, Node.create 1 sink ] |> Map

        f nodes [] [ State.singleton source, (0, 0) ]

    static member length g = List.length g.Nodes

    static member undirected g =
        let edges = Array.create (Graph.length g) []

        for a in [ 0 .. Array.length edges - 1 ] do
            edges[a] <- edges[a] @ g.Edges[a]

            for (b, c) in g.Edges[a] do
                edges[b] <- (a, c) :: edges[b]

        { g with Edges = edges }

let parseLine line =
    line
    |> Seq.map (function
        | '#' -> Forest
        | '.' -> Path
        | '^' -> Slope(North)
        | '>' -> Slope(East)
        | 'v' -> Slope(South)
        | '<' -> Slope(West)
        | _ -> failwith "Invalid char")

let parseTrails lines = lines |> Seq.map parseLine |> array2D

let visit visited i = visited ||| (1UL <<< i)

let notIn visited i = visited &&& (1UL <<< i) = 0UL

let maxPath graph =
    let rec dfs visited a c =
        if a = 1 then
            c
        else
            graph.Edges[a]
            |> List.filter (fun (b, _) -> notIn visited b)
            |> List.map (fun (b, d) -> dfs (visit visited b) b (c + d))
            |> List.fold (fun acc best -> max acc best) 0

    dfs (visit 0UL 0) 0 0

let part1 = maxPath

let part2 = Graph.undirected >> maxPath

let run =
    printfn "== Day 23 =="

    let trails = File.ReadLines("inputs/day23.txt") |> parseTrails
    let graph = Graph.create trails

    printfn "Part 1: %d" (part1 graph)
    printfn "Part 2: %d" (part2 graph)
    printfn ""

module IterationTools

let rowColumn width height =
    [ for y in 0 .. height - 1 do
          [ for x in 0 .. width - 1 do
                yield (x, y) ] ]

let columnRow width height =
    [ for x in 0 .. width - 1 do
          [ for y in 0 .. height - 1 do
                yield (x, y) ] ]

let combinations items =
    let length = List.length items

    [ for i in 0 .. length - 1 do
          for j in i + 1 .. length - 1 do
              yield (items[i], items[j]) ]

let column a c =
    [ for r in 0 .. Array2D.length1 a - 1 do
          yield a[r, c] ]

let columns a =
    [ 0 .. Array2D.length2 a - 1 ] |> List.map (column a)

let row pattern r =
    [ for c in 0 .. Array2D.length2 pattern - 1 do
          yield pattern[r, c] ]

let rows a =
    [ 0 .. Array2D.length1 a - 1 ] |> List.map (row a)

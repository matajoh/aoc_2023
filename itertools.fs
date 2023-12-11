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

def partialSolutions(size: Int32, col: Int32, previous: List[(Int32, Int32)]): List[List[(Int32, Int32)]] & Impure =
    let queens = project previous into Queen;
    let rules = #{
        Row(0).
        Row(y + 1) :- Row(y), if y >= 0 and y + 1 < size.
        Forbidden(y2) :- Row(y2), Queen(x1, y1), if y1 == y2 or Int32.abs(y1 - y2) == Int32.abs(x1 - col).
        Possible(col, y2) :- Row(y2), not Forbidden(y2).
    };
    query queens, rules select (x, y) from Possible(x, y) |> 
        Array.toList |>
        List.map(candidate -> candidate :: previous) 

def main(): Unit & Impure =
    let cols = 8;
    let solutions = List.range(0, cols) |>
        List.foldLeft(
            (previous, col) ->
                previous |> List.flatMap(partialSolutions(cols, col)),
            List.point(Nil));
    println(List.length(solutions))

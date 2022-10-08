type alias Position = (Int32, Int32)

def partialSolutions(size: Int32, col: Int32, previous: List[Position]): List[List[Position]] =
    let queens = project previous into Queen;
    let rows = project List.range(0, size) into Row;
    let rules = #{
        Forbidden(y2) :- Row(y2), Queen(x1, y1), if y1 == y2 or Int32.abs(y1 - y2) == Int32.abs(x1 - col).
        Possible(col, y2) :- Row(y2), not Forbidden(y2).
    };
    query queens, rows, rules select (x, y) from Possible(x, y) |> 
        List.map(candidate -> candidate :: previous) 

def main(): Unit & Impure =
    let size = 8;
    let solutions = List.range(0, size) |>
        List.foldLeft((previous, col) -> List.flatMap(partialSolutions(size, col), previous),
            List.point(Nil));
    println(List.length(solutions))

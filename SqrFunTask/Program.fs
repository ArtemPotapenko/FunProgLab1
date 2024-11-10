module Sqr

let sumFirst n = n * n * (n - 1) / 2

let printInt = printf "%d \n"

let rec recursionSolution n =
    match n with
    | 1 -> 0
    | _ -> recursionSolution (n - 1) + 2 * sumFirst n

recursionSolution 100 |> printInt

let rec tailRecursionSolution n =
    let rec helper helpSum k =
        if k = 0 then
            helpSum
        else
            helper (helpSum + 2 * sumFirst k) (k - 1)

    helper 0 n

tailRecursionSolution 100 |> printInt

let sumFirstList n = [ 1 .. (n - 1) ] |> List.sum |> (*) n

let moduleSolution n =
    [ 1..n ] |> List.fold (fun acc k -> acc + 2 * sumFirstList k) 0

moduleSolution 100 |> printInt


let mapSolution n =
    [ 1..n ] |> List.map sumFirstList |> List.sum |> (*) 2

mapSolution 100 |> printInt

let cycleSolution n =
    seq { for i in 1..n -> 2 * sumFirstList i } |> Seq.sum

cycleSolution 100 |> printInt

let infSolution n =
    Seq.initInfinite ((+) 1)
    |> Seq.map sumFirstList
    |> Seq.take n
    |> Seq.sum
    |> (*) 2

infSolution 100 |> printInt

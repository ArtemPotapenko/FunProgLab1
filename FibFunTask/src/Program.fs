module Fib
let printInt = printf "%d \n"

let fibList n =

    let rec nextFib n =
        match n with
        | 1 -> 1I
        | 0 -> 0I
        | _ -> nextFib (n - 1) + nextFib (n - 2)

    [ 0 .. 10 * n ] |> List.map nextFib

let recursionSolution n =
    let b = 1I
    let a = 0I

    let rec help a b n =
        if a + b > pown 10I (n - 1) then
            2
        else
            1 + (help b (a + b) n)

    help a b n

let tailRecursionSolution n =
    let rec help a b count n =
        if a + b > pown 10I (n - 1) then
            count
        else
            (help b (a + b) (count + 1) n)

    help 0I 1I 2 n

tailRecursionSolution 1000 |> printInt

let moduleFibSolution n =
    let arr = fibList n

    [ 1 .. 10 * n ]
    |> List.fold (fun acc x -> if arr[x] < pown 10I (n - 1) then acc + 1 else acc) 1

moduleFibSolution 1000 |> printInt


let mapFibSolution n =
    fibList n |> List.filter (fun x -> x < pown 10I (n - 1)) |> List.length


mapFibSolution 1000 |> printInt

let cycleFibSolution n =

    let rec nextFib n =
        match n with
        | 1 -> 1I
        | 0 -> 0I
        | _ -> nextFib (n - 1) + nextFib (n - 2)

    seq { for i in 0 .. 10 * n -> nextFib i }
    |> Seq.filter (fun x -> x < pown 10I (n - 1))
    |> Seq.length

cycleFibSolution 1000 |> printInt

let infFibSolution n =

    let rec nextFib n =
        match n with
        | 1 -> 1I
        | 0 -> 0I
        | _ -> nextFib (n - 1) + nextFib (n - 2)

    Seq.initInfinite ((+) 0)
    |> Seq.map nextFib
    |> Seq.take (10 * n)
    |> Seq.filter (fun x -> x < pown 10I (n - 1))
    |> Seq.length

infFibSolution 1000 |> printInt


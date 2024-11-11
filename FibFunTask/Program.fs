module Fib

let printInt = printf "%d \n"


let fibList n =

    let mutable a = 0I
    let mutable b = 1I

    let nextFib x y =
        b <- x + y
        a <- y
        b

    [ 0 .. 10 * n ]
    |> List.map (fun x ->
        match x with
        | 0 -> 0I
        | 1 -> 1I
        | _ -> nextFib a b)


let recursionSolution n =
    let mutable a = 0I
    let mutable b = 1I

    let rec help a b n =
        if a + b > pown 10I (n - 1) then
            2
        else
            1 + (help b (a + b) n)

    help a b n

recursionSolution 100 |> printInt

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
    let mutable a = 0I
    let mutable b = 1I

    let nextFib x y =
        b <- x + y
        a <- y
        b

    seq {
        for i in 0 .. 10 * n ->
            match i with
            | 0 -> 0I
            | 1 -> 1I
            | _ -> nextFib a b
    }
    |> Seq.filter (fun x -> x < pown 10I (n - 1))
    |> Seq.length

cycleFibSolution 1000 |> printInt

let infFibSolution n =
    let mutable a = 0I
    let mutable b = 1I

    let nextFib x y =
        b <- x + y
        a <- y
        b

    Seq.initInfinite ((+) 0)
    |> Seq.map (fun x ->
        if x = 0 then 0I
        else if x = 1 then 1I
        else nextFib a b)
    |> Seq.take (10 * n)
    |> Seq.filter (fun x -> x < pown 10I (n - 1))
    |> Seq.length

infFibSolution 1000 |> printInt

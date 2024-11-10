# Лабораторная работа №1

### Потапенко Артем 336773. Вариант 6, 25

## Задача 6

Найти разность между суммой квадратов первых ста натуральных чисел и квадратом суммы

## Задача 25

Найти индекс первого члена в последовательности Фибоначчи, который должен содержать 1000 цифр.

## Релиазицая на C#

### Задача 6

```C#
   int sum = 0;
        for (int i = 0; i <= 100; i++)
        {
            for (int j = 0; j < i; j++)
            {
                sum += j * i * 2;
            }
        }   
        Console.WriteLine(sum);
```

### Задача 25

```C#
public static void Main(string[] args)
    {
        BigInteger a = 0;
        BigInteger b = 1;
        int count = 1;
        while (b.ToString().Length < 1000)
        {
            count++;
            b = BigInteger.Add(a, b);
            a = BigInteger.Subtract(b, a);
        }

        Console.WriteLine(count);
    }
```

## Реализация Хвостовой рекурсией

### Задача 6

Функция sum высчитывает все парные произведения чисел от 1 до n.
С помощью хвостовой рекурсии высчитываем все удвоенные суммы.

```F#
let sum n = n * n * (n - 1) / 2

let res n =
    let rec helper k helpSum = if k = 0 then helpSum else helper (k - 1) (helpSum + 2 * sum k)
    helper n 0
res 100 |> printf "%d \n"
```

### Задача 25

Функция sum принимает в качестве первых двух аргументов числа Фибоначи, таким образом рекурсивно можем пересчитывает
следующие числа и увеличивать
счетчие найденных чисел с меньшим количеством знаков.

```F#
let rec sum a b count n =
    if a + b => pown 10I (n - 1)   then count else (sum b (a + b) (count + 1) n)
let res = sum 0I 1I 2

res 1000 |> printf "%d \n"
```

## Реализвция рекурсией

### Задача 6

```F#
let sum n = n * n * (n - 1) / 2
let rec result n = if n = 1 then 0 else result (n - 1) + 2 * sum n

result 100 |> printf "%d \n"
```

### Задача 25

При запуске данной программы получаем переполнение стека

```F#
let rec sum a b n =
    if a + b > pown 10I (n - 1)   then 2 else 1 + (sum b (a + b) n)
let res = sum 0I 1I

res 1000 |> printf "%d \n"
```

## Реализвция генерацией

### Задача 6

```F#

let sum n = [ 1 .. (n - 1) ] |> List.sum |> (*) n
let res n = [ 1..n ] |> List.fold (fun acc k -> acc +  2 * sum (k - 1)) 0

res 100 |> printf "%d \n"

```

### Задача 25

```F#
let mutable a = 0I
let mutable b = 1I

let f x y =
    b <- x + y
    a <- y
    b

let fib n =
    [ 0 .. 10 * n ]
    |> List.map (fun x ->
        if x = 0 then 0I
        else if x = 1 then 1I
        else f a b)

let res n =
    let arr = fib n

    [ 1 .. 10 * n ]
    |> List.fold (fun acc x -> if arr[x] < pown 10I (n - 1) then acc + 1 else acc) 1

res 1000 |> printf "%d"
```

## Реализвция c помощью map

### Задача 6

```F#
let sum n = [ 1 .. (n - 1) ] |> List.sum |> (*) n
let res n = [ 1..n ] |> List.fold (fun acc k -> acc +  2 * sum (k - 1)) 0

res 100 |> printf "%d \n"
```

### Задача 25

```F#

let mutable a = 0I
let mutable b = 1I

let f x y =
    b <- x + y
    a <- y
    b


let res n = 
    [ 0..10 * n ]
    |> List.map (fun x ->
        if x = 0 then 0I
        else if x = 1 then 1I
        else f a b) |> List.filter (fun x -> x < pown 10I (n - 1)) |> List.length 
    

res 1000 |> printf "%d \n"
```

## Реализвция c помощью цикла

### Задача 6

```F#

let sum n = [ 1 .. (n - 1) ] |> List.sum |> (*) n
let res n = seq{for i in 1..n -> 2 * sum i} |> Seq.sum

res 100 |> printf "%d \n"

```

### Задача 25

```F#



let mutable a = 0I
let mutable b = 1I

let f x y =
    b <- x + y
    a <- y
    b

let res n =
    seq { for i in 0 .. 10 * n -> (fun i -> if i = 1 then 1I else if i = 0 then 0I else f a b) i }
    |> Seq.filter (fun x -> x < pown 10I (n - 1))
    |> Seq.length

res 1000 |> printf "%d"
```

## Бесконечные последовательности

### Задача 6

```F#
let sum n = [ 1 .. (n - 1) ] |> List.sum |> (*) n

Seq.initInfinite ((+) 1)
|> Seq.map sum
|> Seq.take 100
|> Seq.sum
|> (*) 2
|> printf "%d \n"

```

### Задача 25

```F#
let mutable a = 0I
let mutable b = 1I

let f x y =
    b <- x + y
    a <- y
    b


let res n =
    Seq.initInfinite ((+) 0)
    |> Seq.map (fun x ->
        if x = 0 then 0I
        else if x = 1 then 1I
        else f a b)
    |> Seq.take (10 * n)
    |> Seq.filter (fun x -> x < pown 10I (n - 1))
    |> Seq.length

res 1000 |> printf "%d "
```

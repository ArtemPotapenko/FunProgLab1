module FibFunTask.Test

open NUnit.Framework
open Fib

[<SetUp>]
let Setup () = ()

[<Test>]
let Test1 () =
    Assert.AreEqual(recursionSolution 2, 7)
    Assert.AreEqual(cycleFibSolution 2, 7)
    Assert.AreEqual(infFibSolution 2, 7)

[<Test>]
let Test2 () =
    Assert.AreEqual(mapFibSolution 3, 12)
    Assert.AreEqual(tailRecursionSolution 3, 12)
    Assert.AreEqual(moduleFibSolution 3, 12)

[<Test>]
let Test3 () =
    [ 2..20 ]
    |> List.iter (fun i ->
        Assert.AreEqual(mapFibSolution i, moduleFibSolution i)
        Assert.AreEqual(recursionSolution i, infFibSolution i)
        Assert.AreEqual(tailRecursionSolution i, infFibSolution i)
        Assert.AreEqual(mapFibSolution i, tailRecursionSolution i))
    

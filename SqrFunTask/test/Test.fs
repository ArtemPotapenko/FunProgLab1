module SqrFunTask.test.Test

open NUnit.Framework
open Sqr

[<SetUp>]
let Setup () = ()

[<Test>]
let Test1 () =
    Assert.AreEqual(recursionSolution 2, 4)
    Assert.AreEqual(cycleSolution 2, 4)
    Assert.AreEqual(infSolution 2, 4)

[<Test>]
let Test2 () =
    Assert.AreEqual(mapSolution 3, 36 - 14)
    Assert.AreEqual(tailRecursionSolution 3, 36 - 14)
    Assert.AreEqual(moduleSolution 3, 36 - 14)

[<Test>]
let Test3 () =
    [ 2..20 ]
    |> List.iter (fun i ->
        Assert.AreEqual(mapSolution i, moduleSolution i)
        Assert.AreEqual(recursionSolution i, infSolution i)
        Assert.AreEqual(tailRecursionSolution i, infSolution i)
        Assert.AreEqual(mapSolution i, tailRecursionSolution i))
    

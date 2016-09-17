open System

open GeneticAlgorithm.GlobalParameters
open GeneticAlgorithm.GeneticOperators

[<EntryPoint>]
let main argv = 
    let mutable genNum = 1 
    let mutable Population = Populate MaxPop
    
    while true do
        Population <- Evaluate genNum Population |> 
            Select |>
            Mutate |> 
            AssembleNextGeneration Population
        genNum <- genNum + 1

    Console.ReadKey() |> ignore
    0
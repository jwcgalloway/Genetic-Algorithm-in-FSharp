namespace GeneticAlgorithm

open System

module GlobalParameters =
    
    // Gene:  A subsequence of four steps;  one for each up/down, left/right direction)
    type Gene = 
        { 
            Up: int
            Down: int
            Right: int
            Left: int
        } 

    // Chromosome:  The entire sequence of steps an individual makes in its lifetime
    type Chromosome = 
        { 
            StepSeq : Gene list 
            mutable Fitness : float
            mutable YPos : int
            mutable XPos : int
            mutable StepsTaken : int
        }

    let MaxPop = 50 // The maximum number of individuals in the population
    let NumGenes = 150 // The number of genes the individuals will have - this value dicates the amount of steps an individual takes in their lifetime

    let seed = System.Random()

    let SpaceYLen = 18 // The vertical length of the evaluation area
    let SpaceXLen = 20 // The horizontal length of the evaluation area

    // Maze 1
//    let CheesePos = (2, 17) // The position of the coveted cheese
//    let XWalls = 
//        Map.add 0 (0, 18) Map.empty |> 
//        Map.add 3 (0, 18) 
//
//    let YWalls = 
//        Map.add 0 (1, 3) Map.empty |>
//        Map.add 19 (1, 3)

    // Maze 2
//    let CheesePos = (14, 17) // The position of the coveted cheese
//
//    let XWalls = 
//        Map.add 0 (0, 18) Map.empty |> 
//        Map.add 3 (0, 15) |>
//        Map.add 15 (17, 18)
//
//    let YWalls = 
//        Map.add 0 (1, 3) Map.empty |>
//        Map.add 16 (4, 15) |>
//        Map.add 19 (1, 15) 

    // Maze 3
    let CheesePos = (14, 18) // The position of the coveted cheese

    let XWalls = 
        Map.add 0 (0, 8) Map.empty |> 
        Map.add 3 (0, 5) |> 
        Map.add 6 (9, 13) |> 
        Map.add 9 (8, 10) |> 
        Map.add 16 (6, 7) |>
        Map.add 15 (14, 16) |> 
        Map.add 18 (12, 19) |>
        Map.add 12 (18, 19)

    let YWalls = 
        Map.add 0 (1, 3) Map.empty |>
        Map.add 6 (4, 15) |>
        Map.add 8 (10, 16) |>  
        Map.add 9 (1, 6) |> 
        Map.add 11 (10, 18) |> 
        Map.add 14 (7, 15) |> 
        Map.add 17 (13, 15) |> 
        Map.add 20 (13, 21)


    let X = Map.fold (fun coordAcc Y wallLen -> 
        let coords = List.init (snd wallLen + 1 - fst wallLen + 1) (fun x -> (Y, fst wallLen + x))
        List.concat[coordAcc;coords]) [] XWalls
        
    let Y = Map.fold (fun coordAcc X wallLen ->
        let coords = List.init (snd wallLen + 1 - fst wallLen + 1) (fun y -> (fst wallLen + y, X))
        List.concat[coordAcc;coords]) [] YWalls

    let WallSpace = List.concat[X;Y]
    let mutable PrevAvg = 0.0
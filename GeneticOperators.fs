namespace GeneticAlgorithm

open GeneticAlgorithm.GlobalParameters
open GeneticAlgorithm.HelperMethods

module GeneticOperators =
    let seed = System.Random()

    // Creates a new population of a given size, with a given number of genes
    let Populate popSize =
        Seq.fold (fun popAcc i -> 
            let genes = List.init NumGenes (fun _ -> { Up = seed.Next(2); Down = seed.Next(2); Left = seed.Next(2); Right = seed.Next(2); })
            let chromosome = { StepSeq = genes; Fitness = 0.0; YPos = 1; XPos = 1; StepsTaken = 0; }
            Map.add i chromosome popAcc) Map.empty (seq { 0 .. popSize - 1 })
    // end Populate


    // Evaluates the population by stepping through their solutions and 
    // assigning a fitness score based on their performance
    let Evaluate genNum gen =
        DrawSpace genNum 0 gen
        for i in 0 .. NumGenes - 1 do
            Map.iter (fun individual chromosome -> 
                let gene = chromosome.StepSeq.[i]
                let mutable YPos = chromosome.YPos
                let mutable XPos = chromosome.XPos

                if not ((YPos,XPos) = CheesePos) then
                    if gene.Up = 1 && YPos > 1 then
                        let upOutBounds = List.exists (fun wall -> (YPos - 1, XPos) = wall) WallSpace
                        if not upOutBounds then
                            YPos <- YPos - 1
                    if gene.Down = 1 && YPos < SpaceYLen then
                        let downOutBounds = List.exists (fun wall -> (YPos + 1, XPos) = wall) WallSpace
                        if not downOutBounds then
                            YPos <- YPos + 1
                    if gene.Left = 1  && XPos > 1 then 
                        let leftOutBounds = List.exists (fun wall -> (YPos, XPos - 1) = wall) WallSpace                      
                        if not leftOutBounds then
                            XPos <- XPos - 1
                    if gene.Right = 1  && XPos < SpaceXLen then
                        let rightOutBounds = List.exists (fun wall -> (YPos, XPos + 1) = wall) WallSpace
                        if not rightOutBounds then
                            XPos <- XPos + 1
                
                    chromosome.YPos <- YPos
                    chromosome.XPos <- XPos
                    chromosome.StepsTaken <- chromosome.StepsTaken + 1) gen
            DrawSpace genNum (i + 1) gen
        
        CalculateFitness genNum gen
    // end Evaluate

    // Takes pairs of selected chromosomes (parents), splitting and combining their genes to create two new chromosomes (children)
    let Crossover (parents : Chromosome list) =
        let father = parents.[0]
        let mother = parents.[1]

        let crossPoint = seed.Next(NumGenes)

        let partFather = TakeBefore crossPoint father.StepSeq [] 0
        let partMother = TakeAfter crossPoint mother.StepSeq 0
        let fstChild = List.concat [partFather;partMother]

        let partMother = TakeBefore crossPoint mother.StepSeq [] 0
        let partFather = TakeAfter crossPoint father.StepSeq 0
        let sndChild = List.concat [partMother;partFather]
            
        [fstChild;sndChild]
    // end Crossover


    // Selects individuals for crossover using a roulette wheel selection algorithm
    let Select (gen : Map<int,Chromosome>) =
        let mutable cumulativeFitness = [gen.[0].Fitness]
        for i in 1 .. gen.Count - 1 do
            cumulativeFitness <- cumulativeFitness @ [(cumulativeFitness.[i - 1] + gen.[i].Fitness)]

        let mutable newPop = []
        while newPop.Length < MaxPop - 2 do
            let mutable parents = []
            while parents.Length < 2 do
                let randFitness = seed.NextDouble() * cumulativeFitness.[cumulativeFitness.Length - 1]
                let index = List.findIndex (fun fitness -> fitness > randFitness) cumulativeFitness
                let parent = List.exists (fun chromosome -> chromosome = gen.[index]) parents
                if not parent then
                    parents <- gen.[index]::parents
            let children = Crossover parents
            newPop <- newPop @ children
        newPop
    // end Select


    // Randomly mutate a list of genes
    let Mutate children =
        List.fold (fun mutateAcc child -> 
            let mutatedGenes = 
                List.fold (fun genesAcc gene -> 
                if seed.NextDouble() < 0.005 then
                    let newGene = { Up = seed.Next(2); Down = seed.Next(2); Left = seed.Next(2); Right = seed.Next(2); }
                    newGene::genesAcc
                else gene::genesAcc) [] child
            (Reverse mutatedGenes)::mutateAcc) [] children
    // end Mutate


    let AssembleNextGeneration (gen : Map<int,Chromosome>) (children : Gene list list) = 
        // Elitism
        let blankChrom = { StepSeq = []; Fitness = 0.0; YPos = 1; XPos = 1; StepsTaken = 0; } 
        let blankElite = Map.add 0 blankChrom Map.empty |> Map.add 1 blankChrom

        let elite = Map.fold (fun (elite : Map<int,Chromosome>) individual chromosome -> 
            if chromosome.Fitness > elite.[0].Fitness then 
                Map.add 0 chromosome elite
            else if chromosome.Fitness > elite.[1].Fitness then
                Map.add 1 chromosome elite
            else elite) blankElite gen

        elite.[0].YPos <- 1; elite.[0].XPos <- 1; elite.[0].StepsTaken <- 0;
        elite.[1].YPos <- 1; elite.[1].XPos <- 1; elite.[1].StepsTaken <- 0;

        let mutable i = elite.Count - 1

        List.fold (fun nextGen child -> 
            i <- i + 1 
            let chromosome = { StepSeq = child; Fitness = 0.0; YPos = 1; XPos = 0; StepsTaken = 0;} 
            Map.add i chromosome nextGen) elite children
    // end AssembleNextGeneration
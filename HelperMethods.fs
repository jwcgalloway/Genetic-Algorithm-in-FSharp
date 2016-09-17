namespace GeneticAlgorithm

open System
open System.IO
open System.Threading

open GeneticAlgorithm.GlobalParameters

module HelperMethods =
  
    let GetTotalFitness gen =
        Map.fold (fun total individual chromosome -> total + chromosome.Fitness) 0.0 gen
    // end GetTotalFitness


    // Draw the evaluation area, complete with the current positions of each individual
    let DrawSpace genNum stepNum gen =
        Console.Clear()

        let sb = new Text.StringBuilder(String.Format("\n   Generation: {0}    Previous Fitness: {1:0.00}    Step: {2}\n\n         ", genNum, PrevAvg, stepNum)) 
        let str = 
            Seq.fold (fun (sb : Text.StringBuilder) y -> 
                let segment =
                    Seq.fold (fun line x -> 
                        let isOccupied = Map.exists (fun _ chromosome -> (y,x) = (chromosome.YPos, chromosome.XPos)) gen
                        if isOccupied && (y,x) = CheesePos then
                            line + " !"
                        else if isOccupied then
                            line + " M"
                        else if (y,x) = CheesePos then
                            line + " C"

                        // Draw walls
                        else if XWalls.ContainsKey(y) then // X Walls
                            if x >= (fst XWalls.[y]) && x <= (snd XWalls.[y]) then
                                line + "__"
                            else if YWalls.ContainsKey(x) then
                                if y >= (fst YWalls.[x]) && y <= (snd YWalls.[x]) then
                                    line + "| "
                                else line + "  "
                            else line + "  "

                        else if YWalls.ContainsKey(x) then// Y Walls
                            if y >= (fst YWalls.[x]) && y <= (snd YWalls.[x]) then
                                line + "| "
                            else line + "  "
                        else line + "  ") "" (seq { 0 .. SpaceXLen })
                        
                sb.Append(segment + "\n         ")) sb (seq { 0 .. SpaceYLen }) |> fun x -> x.ToString()
        printfn "%s" str
        Thread.Sleep(75)
    // end DrawSpace
   

    let GetDistance pos =
        let YDist = abs (fst pos - fst CheesePos)
        let XDist = abs (snd pos - snd CheesePos)
        -YDist + -XDist
    // end GetDistance


    let LogAverageFitness genNum gen =
        let fullPath = "C:\Users\Owner\Dropbox\Uni\CAB402 - Programming Paradigms\Assignment2\GeneticAlgorithm\GeneticAlgorithm\logs\AverageFitness.csv"
        use wr = new StreamWriter(fullPath, true)
        PrevAvg <- (GetTotalFitness gen) / float gen.Count
        let log = String.Format("{0},{1}", genNum, PrevAvg)
        wr.WriteLine(log)
        printfn "   Average Fitness: %.2f" PrevAvg
        Thread.Sleep(1000)
        gen
    // end LogAverageFitness


    let CalculateFitness genNum gen =
        Map.iter (fun individual chromosome -> 
            // Award Extra Fitness for speed
            let speedBonus = (NumGenes - chromosome.StepsTaken) * 5
            let fitness = float ((fst CheesePos + snd CheesePos) + (GetDistance (chromosome.YPos, chromosome.XPos))) + float speedBonus
            chromosome.Fitness <- fitness) gen
        LogAverageFitness genNum gen
    // end CalculateFitness


    let rec Reverse L =
        match L with
        | [] -> []
        | [head] -> [head]
        | head::tail -> (Reverse tail) @ [head]
    // end Reverse


    let rec TakeBefore index L acc i =
        if i = index then Reverse acc else
            match L with
            | head::tail -> TakeBefore index tail (head::acc) (i + 1)
            | [] -> Reverse acc
    // end TakeBefore


    let rec TakeAfter index L i =
        if i = index then L else
            match L with
            | head::tail -> TakeAfter index tail (i + 1)
            | [] -> L
    // end TakeAfter
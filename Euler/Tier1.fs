namespace Euler

    module Tier1 = 
        
        let problem1 =
            [1 .. 999] |> List.filter(fun x -> x % 3 = 0 || x % 5 = 0) |> List.fold(fun acc x -> acc + x) 0 

        let problem2 =
            let rec fibutil num state idx targetIdx = 
                match num with 
                | 1 -> 1
                | 2 -> 2
                | _ -> 0
            0
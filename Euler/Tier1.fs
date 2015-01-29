namespace Euler

open System

    module Tier1 = 
        
        let problem1 = [1 .. 999] |> List.filter(fun x -> x % 3 = 0 || x % 5 = 0) |> List.fold(fun acc x -> acc + x) 0 



        let problem2 =
            let rec fibutil num state target acc = 
                if num > target then acc
                else fibutil (num + state) num target (if num % 2 = 0 then acc + num else acc)
            
            fibutil 1 1 4000000 0



//        let quadSieve n =
//            let r = sqrt(n)

        let problem3 = 0



        let rec cartesianProd xl yl = 
            match xl, yl with
            | x::xs, y::ys -> ((x*y)::List.map (fun t -> x*t) ys) @ cartesianProd xs yl
            | _ -> []

        let isPalindrom sequence = 
            let charr = Array.map (fun (x: char) -> int x) (sequence.ToString().ToCharArray())
            charr = Array.rev charr

        let problem4 = 
            (cartesianProd [100 .. 999] [100 .. 999]) |> List.filter (fun x -> isPalindrom x) |> List.max


            
        let rec gcd(x: int64, y: int64) = 
            if x = y then x
            else if x > y then gcd(x-y, y)
            else gcd(x, y-x)
        
        let lcm(x:int64, y: int64) = 
            (x*y) / gcd(x, y)

        let problem5 = 
            [1L..20L] |> List.fold (fun state x -> lcm(state, x)) 1L



        let problem6 = 
            let numbers = [1..100]
            let sum list = List.fold (fun state x -> state + x) 0 list
            let sumOfSquares = numbers |> List.map (fun x -> x*x) |> sum
            let sumOfNumbers = sum numbers

            sumOfNumbers * sumOfNumbers - sumOfSquares


        
        let problem7 = 0



        let bigNum = 
            ("731671765313306249192251196744265747423553491949349698352031277450632623957831801698480186947885184385861560789112949495459501737958331952853208805511" +
             "125406987471585238630507156932909632952274430435576689664895044524452316173185640309871112172238311362229893423380308135336276614282806444486645238749" +
             "303589072962904915604407723907138105158593079608667017242712188399879790879227492190169972088809377665727333001053367881220235421809751254540594752243" +
             "525849077116705560136048395864467063244157221553975369781797784617406495514929086256932197846862248283972241375657056057490261407972968652414535100474" +
             "821663704844031998900088952434506585412275886668811642717147992444292823086346567481391912316282458617866458359124566529476545682848912883142607690042" +
             "242190226710556263211111093705442175069416589604080719840385096245544436298123098787992724428490918884580156166097919133875499200524063689912560717606" +
             "0588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450").ToCharArray() |> Array.map(fun x -> Int64.Parse(string x))

        let problem8 = 
            let arrayProd arr = Array.fold (fun s x -> s*x) 1L arr
            let mutable largestProduct = arrayProd bigNum.[..12]
            let mutable window = 0L

            for i in [12..986] do
                window <- (arrayProd bigNum.[i..i+12])
                if window > largestProduct
                then largestProduct <- window

            largestProduct



        let problem9 = 0

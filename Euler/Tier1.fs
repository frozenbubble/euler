namespace Euler

open System
open System.Threading.Tasks
open System.Collections.Generic
open System.Collections.Concurrent

    module Tier1 = 
        type Agent<'T> = MailboxProcessor<'T>
        type long = int64




        //-----------------------------------------------------------------------------------------
        //   Problem 1
        //-----------------------------------------------------------------------------------------
        let problem1 = [1 .. 999] |> List.filter(fun x -> x % 3 = 0 || x % 5 = 0) |> List.fold(fun acc x -> acc + x) 0 




        //-----------------------------------------------------------------------------------------
        //   Problem 2
        //-----------------------------------------------------------------------------------------
        let problem2 =
            let rec fibutil num state target acc = 
                if num > target then acc
                else fibutil (num + state) num target (if num % 2 = 0 then acc + num else acc)
            
            fibutil 1 1 4000000 0



        
        //-----------------------------------------------------------------------------------------
        //   Problem 4
        //-----------------------------------------------------------------------------------------
        let rec cartesianProd xl yl = 
            match xl, yl with
            | x::xs, y::ys -> ((x*y)::List.map (fun t -> x*t) ys) @ cartesianProd xs yl
            | _ -> []

        let isPalindrom sequence = 
            let charr = Array.map (fun (x: char) -> int x) (sequence.ToString().ToCharArray())
            charr = Array.rev charr

        let problem4 = 
            (cartesianProd [100 .. 999] [100 .. 999]) |> List.filter (fun x -> isPalindrom x) |> List.max




        //-----------------------------------------------------------------------------------------
        //   Problem 5
        //-----------------------------------------------------------------------------------------
        let rec gcd(x: int64, y: int64) = 
            if x = y then x
            else if x > y then gcd(x-y, y)
            else gcd(x, y-x)
        
        let lcm(x:int64, y: int64) = 
            (x*y) / gcd(x, y)

        let problem5 = 
            [1L..20L] |> List.fold (fun state x -> lcm(state, x)) 1L




        //-----------------------------------------------------------------------------------------
        //   Problem 6
        //-----------------------------------------------------------------------------------------
        let problem6 = 
            let numbers = [1..100]
            let sum list = List.fold (fun state x -> state + x) 0 list
            let sumOfSquares = numbers |> List.map (fun x -> x*x) |> sum
            let sumOfNumbers = sum numbers

            sumOfNumbers * sumOfNumbers - sumOfSquares




        //-----------------------------------------------------------------------------------------
        //   Problem 8
        //-----------------------------------------------------------------------------------------
        let bigNum = 
            ("731671765313306249192251196744265747423553491949349698352031277450632623957831801698480186947885184385861560789112949495459501737958331952853208805511" +
             "125406987471585238630507156932909632952274430435576689664895044524452316173185640309871112172238311362229893423380308135336276614282806444486645238749" +
             "303589072962904915604407723907138105158593079608667017242712188399879790879227492190169972088809377665727333001053367881220235421809751254540594752243" +
             "525849077116705560136048395864467063244157221553975369781797784617406495514929086256932197846862248283972241375657056057490261407972968652414535100474" +
             "821663704844031998900088952434506585412275886668811642717147992444292823086346567481391912316282458617866458359124566529476545682848912883142607690042" +
             "242190226710556263211111093705442175069416589604080719840385096245544436298123098787992724428490918884580156166097919133875499200524063689912560717606" +
             "0588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450").ToCharArray() |> Array.map(fun x -> Int64.Parse(string x))

        let arrayProd arr = Array.fold (fun s x -> s*x) 1L arr

        let problem8 = 
            let mutable largestProduct = arrayProd bigNum.[..12]
            let mutable window = 0L

            for i in [12..986] do
                window <- (arrayProd bigNum.[i..i+12])
                if window > largestProduct
                then largestProduct <- window

            largestProduct



        //-----------------------------------------------------------------------------------------
        //   Problem 11
        //-----------------------------------------------------------------------------------------
        let grid = [|08; 02; 22; 97; 38; 15; 00; 40; 00; 75; 04; 05; 07; 78; 52; 12; 50; 77; 91; 08;
                     49; 49; 99; 40; 17; 81; 18; 57; 60; 87; 17; 40; 98; 43; 69; 48; 04; 56; 62; 00;
                     81; 49; 31; 73; 55; 79; 14; 29; 93; 71; 40; 67; 53; 88; 30; 03; 49; 13; 36; 65;
                     52; 70; 95; 23; 04; 60; 11; 42; 69; 24; 68; 56; 01; 32; 56; 71; 37; 02; 36; 91;
                     22; 31; 16; 71; 51; 67; 63; 89; 41; 92; 36; 54; 22; 40; 40; 28; 66; 33; 13; 80;
                     24; 47; 32; 60; 99; 03; 45; 02; 44; 75; 33; 53; 78; 36; 84; 20; 35; 17; 12; 50;
                     32; 98; 81; 28; 64; 23; 67; 10; 26; 38; 40; 67; 59; 54; 70; 66; 18; 38; 64; 70;
                     67; 26; 20; 68; 02; 62; 12; 20; 95; 63; 94; 39; 63; 08; 40; 91; 66; 49; 94; 21;
                     24; 55; 58; 05; 66; 73; 99; 26; 97; 17; 78; 78; 96; 83; 14; 88; 34; 89; 63; 72;
                     21; 36; 23; 09; 75; 00; 76; 44; 20; 45; 35; 14; 00; 61; 33; 97; 34; 31; 33; 95;
                     78; 17; 53; 28; 22; 75; 31; 67; 15; 94; 03; 80; 04; 62; 16; 14; 09; 53; 56; 92;
                     16; 39; 05; 42; 96; 35; 31; 47; 55; 58; 88; 24; 00; 17; 54; 24; 36; 29; 85; 57;
                     86; 56; 00; 48; 35; 71; 89; 07; 05; 44; 44; 37; 44; 60; 21; 58; 51; 54; 17; 58;
                     19; 80; 81; 68; 05; 94; 47; 69; 28; 73; 92; 13; 86; 52; 17; 77; 04; 89; 55; 40;
                     04; 52; 08; 83; 97; 35; 99; 16; 07; 97; 57; 32; 16; 26; 26; 79; 33; 27; 98; 66;
                     88; 36; 68; 87; 57; 62; 20; 72; 03; 46; 33; 67; 46; 55; 12; 32; 63; 93; 53; 69;
                     04; 42; 16; 73; 38; 25; 39; 11; 24; 94; 72; 18; 08; 46; 29; 32; 40; 62; 76; 36;
                     20; 69; 36; 41; 72; 30; 23; 88; 34; 62; 99; 69; 82; 67; 59; 85; 74; 04; 36; 16;
                     20; 73; 35; 29; 78; 31; 90; 01; 74; 31; 49; 71; 48; 86; 81; 16; 23; 57; 05; 54
                     01; 70; 54; 71; 83; 51; 54; 69; 16; 92; 33; 48; 61; 43; 52; 01; 89; 19; 67; 48|] |> Array.map(fun x-> int64 x)

        type Reading =
        | Next of int64[] * int
        | End

        let nextHorizontal idx = 
            if idx%20 < 17 then Next(grid.[idx..idx+3], idx+1)
            else if idx < 380 then Next(grid.[idx+3..idx+6], idx+4)
            else End

        let nextVertical idx = 
            if idx%20 > 0 && idx < 340 then Next([|grid.[idx]; grid.[idx+20]; grid.[idx+40]; grid.[idx+60]|], idx+1)
            else if idx < 340 then Next([|grid.[idx+1]; grid.[idx+21]; grid.[idx+41]; grid.[idx+61]|], idx+1)
            else End
            
        let nextDiagonal idx = 
            if idx%20 < 17 && idx < 340 then Next([|grid.[idx]; grid.[idx+21]; grid.[idx+42]; grid.[idx+63]|], idx+1)
            else if idx <320 then Next([|grid.[idx+3]; grid.[idx+24]; grid.[idx+45]; grid.[idx+66]|], idx+1)
            else End

        let nextDiagonalRev idx = 
            if idx%20 > 0 && idx < 340 then Next([|grid.[idx]; grid.[idx+19]; grid.[idx+38]; grid.[idx+57]|], idx+1)
            else if idx <320 then Next([|grid.[idx+3]; grid.[idx+22]; grid.[idx+41]; grid.[idx+60]|], idx+1)
            else End

        let findLargest f reading =
            let rec findUtil f (reading: Reading) largest = 
                match reading with
                | Next(tuple, next) -> 
                    let state = if (arrayProd tuple) > largest then (arrayProd tuple) else largest
                    findUtil f (f next) state
                | End -> largest
            findUtil (f) reading 0L

        let problem11 = 
            let reading1 = Next(grid.[0..3], 0)
            let reading2 = Next([|grid.[3]; grid.[22]; grid.[41]; grid.[60]|], 3)            
            let functions = [findLargest (nextVertical), reading1; findLargest (nextHorizontal), reading1;
                             findLargest (nextDiagonal), reading1; findLargest (nextDiagonalRev), reading2]

            Async.Parallel [for f,r in functions -> async {return f r}] |> Async.RunSynchronously |> Array.max


        
        //-----------------------------------------------------------------------------------------
        //   Problem 12
        //-----------------------------------------------------------------------------------------
        let divisorsOver500 (n: long) =
            let mutable movingLimit = n
            let mutable divisors = []
            let mutable i = 1L

            while i <= n && i <= movingLimit do
                if n%i = 0L then 
                    divisors <- i::(n/i)::divisors
                    movingLimit <- n/i
                    
                i <- i+1L

            int64 divisors.Length
        
        let memorize f = 
            let cache = new System.Collections.Generic.Dictionary<_,_>()
            (fun x ->
                match cache.TryGetValue x with
                | true, v -> v
                | _ -> let v = f x in cache.Add(x, v); v)

        let memorize2 f = 
            let cache = new Dictionary<_,_>()
            (fun x s ->
                match cache.TryGetValue x with
                | true, v -> 
                    v
                | _ -> let v = (f x s)
                       cache.Add(x, v)
                       v)

        let rec triangleMem  = 
            memorize (fun n -> if n = 1L then 1L else  n + triangleMem(n-1L))
            
        let problem12 =
            let mutable i = 2L
            let mutable num = 1L

            while divisorsOver500 num < 500L do
               //printfn "%d" num
               num <- triangleMem i
               i <- i+1L
                
            num

        
        
        //-----------------------------------------------------------------------------------------
        //   Problem 13
        //-----------------------------------------------------------------------------------------
        let largeSumNumbers = [
                "37107287533902102798797998220837590246510135740250";
                "46376937677490009712648124896970078050417018260538";
                "74324986199524741059474233309513058123726617309629";
                "91942213363574161572522430563301811072406154908250";
                "23067588207539346171171980310421047513778063246676";
                "89261670696623633820136378418383684178734361726757";
                "28112879812849979408065481931592621691275889832738";
                "44274228917432520321923589422876796487670272189318";
                "47451445736001306439091167216856844588711603153276";
                "70386486105843025439939619828917593665686757934951";
                "62176457141856560629502157223196586755079324193331";
                "64906352462741904929101432445813822663347944758178";
                "92575867718337217661963751590579239728245598838407";
                "58203565325359399008402633568948830189458628227828";
                "80181199384826282014278194139940567587151170094390";
                "35398664372827112653829987240784473053190104293586";
                "86515506006295864861532075273371959191420517255829";
                "71693888707715466499115593487603532921714970056938";
                "54370070576826684624621495650076471787294438377604";
                "53282654108756828443191190634694037855217779295145";
                "36123272525000296071075082563815656710885258350721";
                "45876576172410976447339110607218265236877223636045";
                "17423706905851860660448207621209813287860733969412";
                "81142660418086830619328460811191061556940512689692";
                "51934325451728388641918047049293215058642563049483";
                "62467221648435076201727918039944693004732956340691";
                "15732444386908125794514089057706229429197107928209";
                "55037687525678773091862540744969844508330393682126";
                "18336384825330154686196124348767681297534375946515";
                "80386287592878490201521685554828717201219257766954";
                "78182833757993103614740356856449095527097864797581";
                "16726320100436897842553539920931837441497806860984";
                "48403098129077791799088218795327364475675590848030";
                "87086987551392711854517078544161852424320693150332";
                "59959406895756536782107074926966537676326235447210";
                "69793950679652694742597709739166693763042633987085";
                "41052684708299085211399427365734116182760315001271";
                "65378607361501080857009149939512557028198746004375";
                "35829035317434717326932123578154982629742552737307";
                "94953759765105305946966067683156574377167401875275";
                "88902802571733229619176668713819931811048770190271";
                "25267680276078003013678680992525463401061632866526";
                "36270218540497705585629946580636237993140746255962";
                "24074486908231174977792365466257246923322810917141";
                "91430288197103288597806669760892938638285025333403";
                "34413065578016127815921815005561868836468420090470";
                "23053081172816430487623791969842487255036638784583";
                "11487696932154902810424020138335124462181441773470";
                "63783299490636259666498587618221225225512486764533";
                "67720186971698544312419572409913959008952310058822";
                "95548255300263520781532296796249481641953868218774";
                "76085327132285723110424803456124867697064507995236";
                "37774242535411291684276865538926205024910326572967";
                "23701913275725675285653248258265463092207058596522";
                "29798860272258331913126375147341994889534765745501";
                "18495701454879288984856827726077713721403798879715";
                "38298203783031473527721580348144513491373226651381";
                "34829543829199918180278916522431027392251122869539";
                "40957953066405232632538044100059654939159879593635";
                "29746152185502371307642255121183693803580388584903";
                "41698116222072977186158236678424689157993532961922";
                "62467957194401269043877107275048102390895523597457";
                "23189706772547915061505504953922979530901129967519";
                "86188088225875314529584099251203829009407770775672";
                "11306739708304724483816533873502340845647058077308";
                "82959174767140363198008187129011875491310547126581";
                "97623331044818386269515456334926366572897563400500";
                "42846280183517070527831839425882145521227251250327";
                "55121603546981200581762165212827652751691296897789";
                "32238195734329339946437501907836945765883352399886";
                "75506164965184775180738168837861091527357929701337";
                "62177842752192623401942399639168044983993173312731";
                "32924185707147349566916674687634660915035914677504";
                "99518671430235219628894890102423325116913619626622";
                "73267460800591547471830798392868535206946944540724";
                "76841822524674417161514036427982273348055556214818";
                "97142617910342598647204516893989422179826088076852";
                "87783646182799346313767754307809363333018982642090";
                "10848802521674670883215120185883543223812876952786";
                "71329612474782464538636993009049310363619763878039";
                "62184073572399794223406235393808339651327408011116";
                "66627891981488087797941876876144230030984490851411";
                "60661826293682836764744779239180335110989069790714";
                "85786944089552990653640447425576083659976645795096";
                "66024396409905389607120198219976047599490197230297";
                "64913982680032973156037120041377903785566085089252";
                "16730939319872750275468906903707539413042652315011";
                "94809377245048795150954100921645863754710598436791";
                "78639167021187492431995700641917969777599028300699";
                "15368713711936614952811305876380278410754449733078";
                "40789923115535562561142322423255033685442488917353";
                "44889911501440648020369068063960672322193204149535";
                "41503128880339536053299340368006977710650566631954";
                "81234880673210146739058568557934581403627822703280";
                "82616570773948327592232845941706525094512325230608";
                "22918802058777319719839450180888072429661980811197";
                "77158542502016545090413245809786882778948721859617";
                "72107838435069186155435662884062257473692284509516";
                "20849603980134001723930671666823555245252804609722";
                "53503534226472524250874054075591789781264330331690";
        ]

        let section startIndex length (strList: string list) =
            strList |> List.map (fun s -> s.Substring(startIndex, length))

        let splitters pieceLength strLength =  List.init (strLength/pieceLength) (fun i -> (section (i*pieceLength) pieceLength))

        let sections = Async.Parallel [for f in (splitters 10 50) -> async {return f(largeSumNumbers) }] |> Async.RunSynchronously |> Array.rev

        let sums = Async.Parallel [for section in sections -> async {return section |> List.map (fun x -> Int64.Parse x) |> List.sum}] 
                   |> Async.RunSynchronously |> Array.toList

        let ignoreInsignificant digits number = number / (pown 10L digits)

        let rec carry state list = 
            match list with
            | [x] -> x + state
            | x::xs -> carry ((x + state) |> ignoreInsignificant 10) xs
            | _ -> failwith("Empty list")

        let problem13 = carry 0L sums



        //-----------------------------------------------------------------------------------------
        //   Problem 14
        //-----------------------------------------------------------------------------------------
        let even (n: int64) = n % 2L = 0L
        let odd n = not (even n)

        let memoRec f = 
            let d = new System.Collections.Generic.Dictionary<_,_>()
            let rec g x = 
                match d.TryGetValue x with
                | true, v -> v
                | _ ->let v = f g x in d.Add(x, v); v
            g

        let collatzLong = 
            memoRec (fun f n -> 
                if n <= 1L then 0
                else 1 + f (if (even n) then n/2L else n * 3L + 1L) )

        let problem14 = 
            {0L .. 999999L}
            |> Seq.map (fun i -> i, collatzLong i)
            |> Seq.maxBy snd
            |> fst
        


        //-----------------------------------------------------------------------------------------
        //   Problem 15
        //-----------------------------------------------------------------------------------------
        let factorial (n: long) =
            if n = 0L then 1L
            else
                let mutable  fac = 1L
                for i in 1L..n do fac <- fac * i
                fac

        let ncr n k = (factorial n) / (( factorial k) * (factorial(n-k)) )

        let rec pascal r c = ncr r c 

        //let problem15 = pascal 40L 20L
        // wolframalpha
        //  /40\
        //  \20/



        //-----------------------------------------------------------------------------------------
        //   Problem 16
        //-----------------------------------------------------------------------------------------
        let mutable number = Array.init 304 (fun i -> if i = 0 then 1 else 0)

        let double (num: int array) = 
            let mutable carry = 0
            for i in 0..num.Length - 1 do
                num.[i] <- carry + 2 * num.[i]
                if num.[i] >= 10 then
                    num.[i] <- num.[i] - 10
                    carry <- 1
                else carry <- 0

        let problem16 = 
            for i in 1..1000 do
                double number
            number|> Array.sum



        //-----------------------------------------------------------------------------------------
        //   Problem 17
        //-----------------------------------------------------------------------------------------
        let digitToString = 
            let table = [|"One"; "Two"; "Three"; "Four"; "Five"; "Six"; "Seven"; "Eight"; "Nine";|]
            (fun digit -> 
                if digit < 1 || digit > 9 then failwith "Not a valid digit"
                else table.[digit - 1])

        let teenToString = 
            let table = [|"Ten"; "Eleven"; "Twelve"; "Thirteen"; "Fourteen"; "Fifteen"; "Sixteen"; "Seventeen"; "Eighteen"; "Nineteen";|]
            (fun number -> 
                table.[number - 10])

        let tensToString = 
            let table = [|"Twenty"; "Thirty"; "Forty"; "Fifty"; "Sixty"; "Seventy"; "Eighty"; "Ninety";|]
            (fun number -> 
                table.[number/10 - 2])

        let hundredToString number = 
            (digitToString (number / 100)) + "Hundred" + if number % 100 <> 0 then "And" else ""

        let rec numToString (number: int) (state: System.Text.StringBuilder) =
            printfn "%s" (state.ToString())
            if number = 0 then state.ToString()
            else
                if number >= 100 then numToString (number-(100*(number/100))) (state.Append(hundredToString number))
                else if number >= 20 then numToString (number-(10*(number/10))) (state.Append(tensToString number))
                else if number >= 10 then numToString 0 (state.Append(teenToString number))
                else numToString 0 (state.Append(digitToString number))

        let problem17 = 
            ([1..999] |> List.map (fun k -> (numToString k (new System.Text.StringBuilder())).Length) 
            |> List.sum) + "OneThousand".Length




        //-----------------------------------------------------------------------------------------
        //   Problem 18: longest path in a special DAG
        //-----------------------------------------------------------------------------------------
        let topOrder = [|[|75|];
                         [|95; 64|];
                         [|17; 47; 82|];
                         [|18; 35; 87; 10|];
                         [|20; 04; 82; 47; 65|];
                         [|19; 01; 23; 75; 03; 34|];
                         [|88; 02; 77; 73; 07; 63; 67|];
                         [|99; 65; 04; 28; 06; 16; 70; 92|];
                         [|41; 41; 26; 56; 83; 40; 80; 70; 33|];
                         [|41; 48; 72; 33; 47; 32; 37; 16; 94; 29|];
                         [|53; 71; 44; 65; 25; 43; 91; 52; 97; 51; 14|];
                         [|70; 11; 33; 28; 77; 73; 17; 78; 39; 68; 17; 57|];
                         [|91; 71; 52; 38; 17; 14; 91; 43; 58; 50; 27; 29; 48|];
                         [|63; 66; 04; 68; 89; 53; 67; 30; 73; 16; 69; 87; 40; 31|];
                         [|04; 62; 98; 27; 23; 09; 70; 98; 73; 93; 38; 53; 60; 04; 23|]|]      

        let problem18 = 
            let paths = Array.map (fun row -> Array.map (fun x -> -1) row) topOrder
            
            let getLongestPath (row, col) = paths.[row].[col]

            let nextNode (row, column) (matrix: int array array) = 
                if column < matrix.[row].Length - 1 then Some (row, column + 1)
                else if row < matrix.Length - 1 then Some (row + 1, 0)
                else None

            let getNeighBours (row, col) (matrix: int array array) =
                if row < matrix.Length - 1 then 
                    Some (row + 1, col), Some (row + 1, col + 1)
                else None, None

            let updateNode (node: (int*int) option) weight = 
                match node with
                | Some (row, col) ->
                     if weight + topOrder.[row].[col] > paths.[row].[col] then 
                         printf "updating node: %d, %d" row col
                         paths.[row].[col] <- weight + topOrder.[row].[col]
                | None -> ()

            let rec traverse (root: (int*int) option) (nodes: int[][]) = 
                match root with
                | Some (row, col) -> 
                    printfn "Processing node: %d, %d" row col
                    let neighbours = getNeighBours (row, col) nodes
                    let left = (fst neighbours)
                    let right = (snd neighbours)
                    updateNode left (getLongestPath (row, col))
                    updateNode right (getLongestPath (row, col))
                    traverse (nextNode (row, col) nodes) nodes
                | None -> ()
            
            updateNode (Some(0, 0)) 75
            traverse (Some(0, 0)) topOrder

            (paths |> Array.collect (id) |> Array.max) - 75



        //-----------------------------------------------------------------------------------------
        //   Problem 19 Counting sundays
        //-----------------------------------------------------------------------------------------
        let sundays = Seq.toList (seq {for i in 1 .. 365025 do if i% 7 =6 then yield i })

        let daysIn month = 
            let mutable dayCount = 0
            match (month%12) with
            | 1 | 3 | 5 | 7 | 8 | 10 | 0 -> dayCount <- 31
            | 4 | 6 | 9 | 11 -> dayCount <- 30
            | _-> dayCount <- 28
            
            if month % 48 = 38 then dayCount <- dayCount + 1
            dayCount

        let firstDays = 
            Seq.unfold (fun (month, acc) -> 
                let days = daysIn(month)
                Some (acc + days + 1, (month + 1, days + acc))) (1, 0)

        let rec merge (xl: int list, yl: int list): int list = 
            match (xl, yl) with
            | ([], yl) -> []
            | (xl, []) -> []
            | (x::xs, y::ys) -> 
                if x < y then merge(xs, y::ys)
                else if x > y then merge(x::xs, ys)
                else x::merge(xs, ys)

        let problem19 = 
            let firsts = firstDays |> Seq.take 1200 |> Seq.toList
            merge(sundays, firsts).Length




        //-----------------------------------------------------------------------------------------
        //   Problem 20 Factorial digit sum
        //-----------------------------------------------------------------------------------------  
        type bint = System.Numerics.BigInteger

        let fac (n: bint) = 
            let rec facUtil (n: bint) (s:bint) =
                if n = 1I then s
                else facUtil (n-1I) (n*s)
            facUtil n 1I

        let problem20 = 
            let sumDigits (num: char array) = 
                let mutable sum = 0
                for digit in num do
                    sum <- sum + (digit |> string |> Int32.TryParse |> snd )
                sum

            (fac 100I).ToString().ToCharArray() |> sumDigits




        //-----------------------------------------------------------------------------------------
        //   Problem 21 Amicable numbers
        //-----------------------------------------------------------------------------------------
        let divisorsOf n = 
            let mutable movingLimit = n
            let mutable divisors = []
            let mutable i = 1

            while i <= n && i <= movingLimit do
                if n%i = 0 then 
                    divisors <- i::(n/i)::divisors
                    movingLimit <- n/i
                i <- i+1
            divisors |> Seq.filter (fun x -> x < n) |> Seq.distinctBy id

        let divisorSumOf n = divisorsOf n |> Seq.sum

        let memory = new System.Collections.Generic.Dictionary<int, int>()
        
        let rec amicables (limit, n, list) =
            if n > limit then list
            else
                let divSum = (divisorSumOf n)
                memory.Add(n, divSum)

                match memory.TryGetValue(divSum) with
                | true, v -> 
                    if v = n then amicables(limit, (n+1), ((n, divSum)::list))
                    else amicables(limit, (n+1), list)
                | _ -> amicables(limit, (n+1), list)

        let problem21= 
            memory.Clear()
            amicables(10000, 1, []) |> List.collect (fun (a, b) -> if a <> b then [a; b] else []) |> Seq.sum




        //-----------------------------------------------------------------------------------------
        //   Problem 22 Names scores
        //-----------------------------------------------------------------------------------------
        let names = 
            let client = new System.Net.WebClient()
            client.DownloadString("https://projecteuler.net/project/resources/p022_names.txt").Replace("\"", "").Split(',')
                |> Array.map (fun (s: string) -> s.Trim())
                |> Array.sort |> Array.map (fun s -> s.ToUpper().ToCharArray() |> Array.sumBy (fun c -> int(c) - int('A') + 1))

        let problem22 = Array.map2 (fun n s -> n * s) names [|1..names.Length|] |> Array.sum




        //-----------------------------------------------------------------------------------------
        //   Problem 23 Non-abundant sums  TODO
        //-----------------------------------------------------------------------------------------
        let abdundant(n) = divisorSumOf(n) > n

        let sumPairsOf(n) = [|1..n|] |> Array.map (fun i -> (n-i, i))

        let bothAbdundant(x, y) = abdundant(x) && abdundant(y)

        let abdundantPairs(n) = sumPairsOf(n) |> Array.filter bothAbdundant

        let problem23 = 0




        //-----------------------------------------------------------------------------------------
        //   Problem 24 Lexicographic permutations
        //-----------------------------------------------------------------------------------------
        let digits = [|0..9|]
        let factors = digits |> Array.map (fun n -> factorial(int64(n)+1L) |> int32) |> Array.rev
        let perms = Array.init 10 (fun i -> 0)

        let findPerm limit =
            let mutable x0 = 0
            for i in 0..9 do
                perms.[i] <- (limit - x0) / factors.[i]
                x0 <- x0 + perms.[i] * factors.[i]

            perms |> Array.toList

        let removeElement arr idx = [(Array.sub arr 0 idx); (Array.sub arr (idx+1) (arr.Length-idx-1))] |> Array.concat

        let rec permsToDigits perms available digits = 
            match perms with
            | [] -> List.concat [(digits |> List.rev); (Array.toList available)]
            | head::tail -> permsToDigits tail (removeElement available head) (available.[head]::digits)

        let problem24 = permsToDigits (findPerm(999999) |> List.tail) digits []




        //-----------------------------------------------------------------------------------------
        //   Problem 25 1000-digit Fibonacci number
        //-----------------------------------------------------------------------------------------
        let limit = bigint(10)**999

        //pen and paper: Binet formula, log function




        //-----------------------------------------------------------------------------------------
        //   Problem 28 Number spiral diagonals
        //-----------------------------------------------------------------------------------------
        let problem28 = (Array.init 501 (fun i -> (pown (bigint(2*(i+1)-1)) 2), bigint(2*i)) |> Array.map (fun (x, d) -> 4I*x - 6I*d) |> Array.sum) - 3I




        //-----------------------------------------------------------------------------------------
        //   Problem 29 Distinct powers
        //-----------------------------------------------------------------------------------------
        // brute force
        let problem29 = 
            [2..100] |> List.collect (fun x -> [2..100] |> List.map (fun y -> (bigint  x) ** y)) 
            |> Seq.distinct |> Seq.length
            



        //-----------------------------------------------------------------------------------------
        //   Problem 31 Coin sums
        //-----------------------------------------------------------------------------------------

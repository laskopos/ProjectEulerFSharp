module TestService

open System

let (|Two|_|) num = 
    if num % 2 = 0 then Some Two
    else None

let (|Five|_|) num = 
    if num % 5 = 0 then Some Five
    else None

let rec FindMultiplies number currentSum = 
    let numToAdd =
        match number with
        | Two -> number
        | Five -> number
        | _ -> 0
        
    let newSum = currentSum + numToAdd
    let newNumber = number + 1
    
    if newNumber < 1000 then
        FindMultiplies newNumber newSum
    else
        newSum

let rec FindSumOfEvenFibonacci (firstValue:int) (secondValue:int) (currentSum:int) =
    let nextValue = firstValue + secondValue

    let newSum = 
        match nextValue with
        | Two -> currentSum + nextValue
        | _ -> currentSum
    
    if nextValue < 4000000 then
        FindSumOfEvenFibonacci secondValue nextValue newSum
    else
        newSum

let rec FindLargestPrimeFactor (numberToDivide:uint64) (currentHighestFactor:uint64) =
    if numberToDivide % currentHighestFactor = System.UInt64.Parse "0" then
        let newNumberToDivide = numberToDivide / currentHighestFactor
        if newNumberToDivide = System.UInt64.Parse "1" then
            currentHighestFactor
        else
            FindLargestPrimeFactor newNumberToDivide currentHighestFactor
    else
        FindLargestPrimeFactor numberToDivide (currentHighestFactor + System.UInt64.Parse "1")

let rec FindLargestPalindromeProductOf3Digits firstFactor secondFactor (largestPalindrome:string) = 
    let product = firstFactor * secondFactor |> string

    let arrayProduct = product.ToCharArray();

    let revProduct = product.ToCharArray() |> Array.rev |> System.String
    
    let newLargestPalindrome =
        if product = revProduct & System.Int32.Parse(product) > System.Int32.Parse(largestPalindrome) then
            product
        else
            largestPalindrome
    
    if firstFactor = 0 & secondFactor = 0 then
        newLargestPalindrome
    elif secondFactor = 0 then
        FindLargestPalindromeProductOf3Digits (firstFactor - 1) 999 newLargestPalindrome
    else
        FindLargestPalindromeProductOf3Digits firstFactor (secondFactor - 1) newLargestPalindrome

let rec FindSmallestEvenlyDivisibleNumber range (currentSmallestNumber:int) = 
    let rec findNewSmallestNumber range currentSmallestNumber = 
        match range with
        | [] -> currentSmallestNumber
        | x::xs ->
            if currentSmallestNumber % x = 0 then
                findNewSmallestNumber xs currentSmallestNumber
            else
                currentSmallestNumber + 1
    
    let newSmallestNumber = findNewSmallestNumber range currentSmallestNumber

    if newSmallestNumber > currentSmallestNumber then
        FindSmallestEvenlyDivisibleNumber range newSmallestNumber
    else
        currentSmallestNumber

let square x = x*x

let rec SumOfSquares range =
    List.sumBy square range

let rec SquareOfSum range =
    range |> List.sum |> square

let IsDivisible number numberInRange =
    number % numberInRange = 0

let isPrime number = 
    [2..number - 1] |> List.filter (IsDivisible number) |> List.length = 0

let rec IteratePrimeNumbers (numberToBegin:int) countToReach =
    
    let nextNumberToIterate = (numberToBegin + 1)
    let newCountToReach = (countToReach + 1)
    if isPrime (numberToBegin) then
        if countToReach <= 10001 then
            IteratePrimeNumbers nextNumberToIterate newCountToReach
        else
            numberToBegin
    else
        IteratePrimeNumbers nextNumberToIterate countToReach


let digitsToProduct inp =
    inp |> Seq.map (string >> bigint.Parse)
        |> Seq.fold (*) 1I

let LassenLargestProduct input num =
    input |> Seq.toList |> Seq.windowed num |> Seq.map digitsToProduct |> Seq.max

LassenLargestProduct "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450" 13

let PythagoreanTriplets sum = 
    [for x in 1 .. sum do
            for y in 1 .. x - 1 do
                let a = x*x - y*y
                let b = 2*x*y
                let c = x*x+y*y
                yield [a;b;c]
            ]

let MultiplyList list = List.fold (*) 1 list

let FindSpecialPythagoreanTriplet sum =
    PythagoreanTriplets sum |> List.find (fun [a;b;c] -> a+b+c = sum) |> MultiplyList

// Problem 10
open System
let findFactorsOf (n:int64) =
    let limit = int64(Math.Sqrt(double(n)))
    [2L .. limit] |> Seq.filter (fun x -> n % x  = 0L)

let isPrimeNum (n:int64) = Seq.isEmpty (findFactorsOf(n))

let primeSequence max = seq {for n in 2L .. max do if isPrimeNum n then yield n}

// Problem 11
let numbers =
    array2D [|
      [|08;02;22;97;38;15;00;40;00;75;04;05;07;78;52;12;50;77;91;08|];
      [|49;49;99;40;17;81;18;57;60;87;17;40;98;43;69;48;04;56;62;00|];
      [|81;49;31;73;55;79;14;29;93;71;40;67;53;88;30;03;49;13;36;65|];
      [|52;70;95;23;04;60;11;42;69;24;68;56;01;32;56;71;37;02;36;91|];
      [|22;31;16;71;51;67;63;89;41;92;36;54;22;40;40;28;66;33;13;80|];
      [|24;47;32;60;99;03;45;02;44;75;33;53;78;36;84;20;35;17;12;50|];
      [|32;98;81;28;64;23;67;10;26;38;40;67;59;54;70;66;18;38;64;70|];
      [|67;26;20;68;02;62;12;20;95;63;94;39;63;08;40;91;66;49;94;21|];
      [|24;55;58;05;66;73;99;26;97;17;78;78;96;83;14;88;34;89;63;72|];
      [|21;36;23;09;75;00;76;44;20;45;35;14;00;61;33;97;34;31;33;95|];
      [|78;17;53;28;22;75;31;67;15;94;03;80;04;62;16;14;09;53;56;92|];
      [|16;39;05;42;96;35;31;47;55;58;88;24;00;17;54;24;36;29;85;57|];
      [|86;56;00;48;35;71;89;07;05;44;44;37;44;60;21;58;51;54;17;58|];
      [|19;80;81;68;05;94;47;69;28;73;92;13;86;52;17;77;04;89;55;40|];
      [|04;52;08;83;97;35;99;16;07;97;57;32;16;26;26;79;33;27;98;66|];
      [|88;36;68;87;57;62;20;72;03;46;33;67;46;55;12;32;63;93;53;69|];
      [|04;42;16;73;38;25;39;11;24;94;72;18;08;46;29;32;40;62;76;36|];
      [|20;69;36;41;72;30;23;88;34;62;99;69;82;67;59;85;74;04;36;16|];
      [|20;73;35;29;78;31;90;01;74;31;49;71;48;86;81;16;23;57;05;54|];
      [|01;70;54;71;83;51;54;69;16;92;33;48;61;43;52;01;89;19;67;48|];
      |]

let calculateProduct (a:int) (b:int) (c:int) (d:int) = a * b * c * d

let horizontalProduct (i, j) = 
    if i > 19 || j > 16 then 0
    else calculateProduct numbers.[i,j] numbers.[i, j + 1] numbers.[i, j + 2] numbers.[i, j + 3]

let verticalProduct (i, j) = 
    if j > 19 || i > 16 then 0
    else calculateProduct numbers.[i,j] numbers.[i + 1, j] numbers.[i + 2, j] numbers.[i + 3, j]

let diagonalRightProduct (i, j) = 
    if i > 16 || j > 16 then 0
    else calculateProduct numbers.[i, j] numbers.[i + 1, j + 1] numbers.[i + 2, j + 2] numbers.[i + 3, j + 3]

let diagonalLeftProduct (i,j) = 
    if i > 16 || j < 3 then 0
    else calculateProduct numbers.[i, j] numbers.[i  + 1 , j - 1] numbers.[i + 2, j - 2] numbers.[i + 3, j - 3]

let findMaxProductInDirection dirCalcFunc = 
    seq{for i in 0 .. 19 do
            for j in 0 .. 19 do
                yield dirCalcFunc (i, j)}
    |> Seq.max

let findMaximumIn2DArray = 
    [|findMaxProductInDirection horizontalProduct;
    findMaxProductInDirection verticalProduct;
    findMaxProductInDirection diagonalLeftProduct;
    findMaxProductInDirection diagonalRightProduct|]

// Problem 12
let triangleNum (num:int64) = [1L .. num] |> Seq.sum

let getDivisors (num:int64) =
    let upperBound = int64(Math.Sqrt(double(num)))
    [1L .. upperBound] 
    |> Seq.filter (fun x -> num % x = 0L)
    |> Seq.collect (fun x -> [x; num/x])

let naturalNumbers = Seq.unfold (fun x -> Some(x, x+1L)) 1L

let findNumberOver500Divisors =
    naturalNumbers
    |> Seq.map (fun x -> triangleNum(x))
    |> Seq.filter (fun y -> Seq.length(getDivisors(y)) > 500)
    |> Seq.head

open System

let getSumFromFile = 
    let lines = IO.File.ReadAllLines("ProjectEulerProblem13.txt")

    lines |> Seq.map  (bigint.Parse) |> Seq.sum

let firstTenDigits = getSumFromFile.ToString().ToCharArray() |> Seq.take(10) |> Seq.toList

open System
open System.Runtime.Serialization
open System.Text
// Problem 14
let getNextNumberForOddNumber num:int64 =
    (3L * num) + 1L

let getNextNumForEvenNumber num:int64 = 
    num/2L

let getNextNumberOfSequence num:int64 = 
    if num % 2L = 0L then
        getNextNumForEvenNumber num
    else
        getNextNumberForOddNumber num

let rec CollatzSequenceCount (currentNum:int64) (count:int) = 
    if currentNum <= 1L then
        count + 1
    else
        // printfn "%i" currentNum
        CollatzSequenceCount (getNextNumberOfSequence currentNum) count + 1

let countSequence = seq{for x in 1L .. 1000000L do yield (x,(CollatzSequenceCount x 0))}

countSequence |> Seq.reduce (fun (x , y) (a,b) -> if b < y then (x,y) else (a,b))

let factorial (num:bigint) : bigint = 
    [1I .. num] |> List.fold (( * )) 1I

// Problem 15
let routes stepsRequired perDirection = 
    factorial stepsRequired / (factorial (perDirection) * (factorial(stepsRequired - perDirection)))

// Problem 16
let powerAsString (x:bigint) (y:int) = bigint.Pow(x, y) |> string

let powerDigitSum x y = (powerAsString x y) |> Seq.map (fun x -> int32(x.ToString())) |> Seq.sum

// Problem 17
let getNumberString value =
    match value with
    | 1 -> "one"
    | 2 -> "two"
    | 3 -> "three"
    | 4 -> "four"
    | 5 -> "five"
    | 6 -> "six"
    | 7 -> "seven"
    | 8 -> "eight"
    | 9 -> "nine"
    | _ -> ""

let getStringForTenths tens ones =
    match tens with
    | 0 -> getNumberString ones
    | 1 ->
        match ones with
        | 1 -> "eleven"
        | 2 -> "twelve"
        | 3 -> "thirteen"
        | 4 -> "fourteen"
        | 5 -> "fifteen"
        | 6 -> "sixteen"
        | 7 -> "seventeen"
        | 8 -> "eighteen"
        | 9 -> "nineteen"
        | 0 -> "ten"
        | _ -> ""
    | 2 -> "twenty" + getNumberString ones
    | 3 -> "thirty" + getNumberString ones
    | 4 -> "forty" + getNumberString ones
    | 5 -> "fifty" + getNumberString ones
    | 6 -> "sixty" + getNumberString ones
    | 7 -> "seventy" + getNumberString ones
    | 8 -> "eighty" + getNumberString ones
    | 9 -> "ninety" + getNumberString ones
    | _ -> ""

let toWord value =
    let thousands = value / 1000
    let hundreds = (value - 1000 * thousands) / 100
    let tens = (value - 1000 * thousands - hundreds * 100) / 10
    let ones = (value - 1000 * thousands - hundreds * 100 - tens * 10)

    let thousandsWord = if thousands > 0 then getNumberString thousands + " thousand " else ""
    let hundredsWord = if hundreds > 0 then getNumberString hundreds + " hundred" else ""
    let tensPrefix = if (thousands > 0 || hundreds > 0) && (tens > 0 || ones > 0) then "and " else ""
    let tensWord = tensPrefix + getStringForTenths tens ones
    // let onesWord = if tens >getNumberString ones

    thousandsWord + hundredsWord + tensWord

let getCountOfLetters (input:string) =
    input.Replace(" ", "").Length

let countOfWords = 
    List.sumBy getCountOfLetters ([1 .. 1000] |> List.map toWord)

open System
// Problem 18
let readLinesFromFile =
    IO.File.ReadAllLines "C:\\temp\\euler_problem18.txt"
    |> Array.map (fun x -> 
                    let trimmed = x.TrimStart ' ' 
                    trimmed.Split ' '  |> Array.map(fun x -> Int32.Parse(x)) |> Array.toList)
    |> Array.toList

let getNewTotal (currentRow:int list) (total: int list) = 
    let head = total.Head
    let tail = List.nth total (total.Length-1)
    let body = total |> Seq.windowed 2 |> Seq.map (fun l -> Seq.max l) |> Seq.toList

    let newList = List.append total [tail]

    List.map2 (+) currentRow newList
    List.map2 (+) currentRow (List.concat [[head]; body; [tail]])

let rec traverse (triangle:int list list) total index =
    let row = triangle.[index]
    let newTotal = getNewTotal row total

    if index < (List.length triangle) - 1 then
        traverse triangle newTotal (index + 1)
    else
        newTotal

let getResult = 
    traverse readLinesFromFile [75] 1
    |> List.max

// Problem 19
let getSundays =
    [1901 .. 2000]
    |> List.collect (fun x -> [1 .. 12] |> List.map(fun y -> DateTime(x, y, 1)))
    |> List.filter (fun x -> x.DayOfWeek = DayOfWeek.Sunday)
    |> List.length

//Problem 20
let getFactorialDigitSum =
    let factorString =
        [1I .. 100I]
        |> List.rev
        |> List.fold (*) 1I
        |> string

    factorString
    Array.sumBy (fun x -> System.Int32.Parse(x.ToString())) (factorString.ToCharArray())
    // |> Array.map (int32))
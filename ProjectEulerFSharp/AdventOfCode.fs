module AdventOfCode
open System
// Day 1

let areNumbersPair (input1:int) (input2:int) =
    input1 = input2

let returnFirstIfPair (boolFunc:int -> int -> bool) x y =
    if boolFunc x y then x else 0

let rec getSumOfParis list  currentIndex (currentSum:int) =
     if currentIndex = List.length list - 1 then
        currentSum + returnFirstIfPair areNumbersPair list.[currentIndex] list.[0]
     else
        currentSum + returnFirstIfPair areNumbersPair list.[currentIndex] list.[currentIndex + 1]
        |> getSumOfParis list (currentIndex + 1) 

let findSumOfPairs (input:string) =
    let intList =
        input
        |> Seq.map (fun x -> Int32.Parse(x.ToString()))
        |> Seq.toList 

    getSumOfParis intList 0 0

let getListCheckSum (list:int list) =
    let max = 
        list
        |> List.max

    let min = 
        list
        |> List.min
    
    max - min

// Day 2
let findListCheckSum (list:List<List<int>>) =
    // let list1 =
        List.sumBy getListCheckSum list
    // list1
    // |> List.sum
let list1 = [5;1;9;5]
let list2 = [7; 5; 3]
let list3 = [2;4;6;8]
findListCheckSum [list1;list2;list3]

// day 3

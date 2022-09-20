#load "Common.fsx"
#r "nuget: Expecto"

open Common
open Expecto

let choose2 n = n * (n - 1) / 2

let nChooseK n k =
    if k > n then
        0.0
    else
        let rec factorial n =
            if n = 0UL then
                1UL
            else
                n * factorial (n - 1UL)

        (float (factorial n))
        / (float ((factorial k) * factorial (n - k)))

let probabilityOfDominantAllele k l m =
    let numberCombinations =
        let total = k + l + m
        choose2 total

    let percentageRecessive =
        let percentageRecessiveOfHeterozygous = 0.25
        let percentageRecessiveOfHeteroAndHomoRec = 0.5

        float (choose2 m)
        + (percentageRecessiveOfHeterozygous
           * float (choose2 l))
        + (percentageRecessiveOfHeteroAndHomoRec
           * float l
           * float m)

    1.0
    - (percentageRecessive / (float numberCombinations))


let tests =
    testList
        "iprb"
        [ test "sample" {
              let output = probabilityOfDominantAllele 2 2 2

              Expect.floatClose Accuracy.low output 0.78333 ""
          } ]

let main = runTestsWithCLIArgs [] [||] tests

probabilityOfDominantAllele 27 18 19

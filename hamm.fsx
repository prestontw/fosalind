#load "Common.fsx"
#r "nuget: Expecto"

open Common
open Expecto


let problem (input: string) =
    let [| first; second |] | [| first; second; _ |] = input.Split "\n"

    Seq.map2 (fun f s -> if f = s then 0 else 1) (first) second
    |> Seq.sum


let tests =
    testList
        "hamm"
        [ test "sample" {
              let output =
                  """GAGCCTACTAACGGGAT
CATCGTAATGACGGCCT"""
                  |> problem

              Expect.equal output 7 ""
          } ]

let main = runTestsWithCLIArgs [] [||] tests

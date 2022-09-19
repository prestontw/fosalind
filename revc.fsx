#load "Common.fsx"
#r "nuget: Expecto"

open Common
open Expecto

let complement =
    function
    | A -> T
    | T -> A
    | G -> C
    | C -> G

let problem (input: string) =
    input
    |> Seq.choose NucleobaseOfChar
    |> Seq.map complement
    |> Seq.rev
    |> Seq.map (fun x -> x.ToString())
    |> String.concat ""


let tests =
    testList
        "revc"
        [ test "sample" {
              let output = "AAAACCCGGT" |> problem

              Expect.equal output "ACCGGGTTTT" ""
          } ]

let main = runTestsWithCLIArgs [] [||] tests

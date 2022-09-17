#load "Common.fsx"
#r "nuget: Expecto"

open Common
open Expecto

let translate (nucleo: Nucleobase) =
    match nucleo with
    | T -> RNA.U
    | A -> RNA.A
    | G -> RNA.G
    | C -> RNA.C

let problem (input: string) =
    input
    |> Seq.choose NucleobaseOfChar
    |> Seq.map translate
    |> Seq.map (fun x -> x.ToString())
    |> String.concat ""


let tests =
    testList
        "rna"
        [ test "sample" {
              let output = "GATGGAACTTGACTACGTAAATT" |> problem

              Expect.equal output "GAUGGAACUUGACUACGUAAAUU" ""
          } ]

let main = runTestsWithCLIArgs [] [||] tests

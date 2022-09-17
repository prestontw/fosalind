#load "Common.fsx"
#r "nuget: Expecto"

open Common
open Expecto

let problem (input: string) =
    let counts = Seq.countBy id input |> Map.ofSeq
    counts

let display (map: Map<char, int>) =
    sprintf "%A %A %A %A" map['A'] map['C'] map['G'] map['T']

let tests =
    testList
        "dna"
        [ test "sample" {
              let output =
                  "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"
                  |> problem
                  |> display

              Expect.equal output "20 12 17 21" ""
          } ]

let main = runTestsWithCLIArgs [] [||] tests

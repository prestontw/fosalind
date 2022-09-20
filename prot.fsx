#load "Common.fsx"
#r "nuget: Expecto"

open Common
open Expecto


let encodeRna input =
    let rnaSeq = input |> Seq.choose RNA.riboBaseOfChar

    rnaSeq
    |> Seq.chunkBySize 3
    |> Seq.map (fun chunk -> RNA.encode chunk[0] chunk[1] chunk[2])
    |> Seq.map (fun x -> x.ToString())
    |> String.concat ""




let tests =
    testList
        "prot"
        [ test "sample" {
              let output =
                  "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA"
                  |> encodeRna

              Expect.equal output ("MAMAPRTEINSTRING" + "Stop") ""
          } ]

let main = runTestsWithCLIArgs [] [||] tests

readData "rosalind_prot.txt" |> encodeRna

#load "Common.fsx"
#r "nuget: Expecto"

open Common
open Expecto

let GCContent (body: string) =
    let length = float body.Length

    let gCount =
        body
        |> String.filter (fun x -> x = 'G')
        |> String.length
        |> float
        |> fun f -> f * 100.0

    let cCount =
        body
        |> String.filter (fun c -> c = 'C')
        |> String.length
        |> float
        |> fun f -> f * 100.0

    (gCount + cCount) / length

let problem input =
    input
    |> parseFasta
    |> Array.map (fun record -> record.id, record.body |> GCContent)
    |> Array.maxBy (fun (_id, percentage) -> percentage)
    |> fun (id, percentage) -> sprintf "%s\n%f" id percentage


let tests =
    testList
        "gc"
        [ test "sample" {
              let output =
                  """>Rosalind_6404
CCTGCGGAAGATCGGCACTAGAATAGCCAGAACCGTTTCTCTGAGGCTTCCGGCCTTCCC
TCCCACTAATAATTCTGAGG
>Rosalind_5959
CCATCGGTAGCGCATCCTTAGTCCAATTAAGTCCCTATCCAGGCGCTCCGCCGAAGGTCT
ATATCCATTTGTCAGCAGACACGC
>Rosalind_0808
CCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACCAGCCCGGAC
TGGGAACCTGCGGGCAGTAGGTGGAAT"""
                  |> problem

              Expect.equal
                  output
                  """Rosalind_0808
60.919540"""
                  ""
          } ]

let main = runTestsWithCLIArgs [] [||] tests

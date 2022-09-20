#load "Common.fsx"
#r "nuget: Expecto"

open Common
open Expecto


type FastaRecord = { id: string; body: string }

let parseFasta (s: string) =
    let lines = s.Split "\n"

    let output =
        Array.fold
            (fun (list, current) line ->
                match Seq.tryHead line, current with
                | None ->
                    Array.append
                        list
                        (match current with
                         | Some l -> [| l |]
                         | None -> [||]),
                    None
                | Some c when c = '>' -> Array.append list [| current |], Some line[1..]
                | Some c ->
                    list,
                    match current with
                    | Some cur -> Some cur ++ line[1..]
                    | None -> Some line[1..])
            ([||], None)
            (lines)
    // Add in progress fasta to final list
    fst output


let tests =
    testList
        "fib"
        [ test "sample" {
              let output = fib 5L 3L

              Expect.equal output 19L ""
          } ]

let main = runTestsWithCLIArgs [] [||] tests

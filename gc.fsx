#load "Common.fsx"
#r "nuget: Expecto"

open Common
open Expecto


type FastaRecord = { id: string; body: string }

let parseFasta (s: string) =
    let lines = s.Split "\n"

    let output =
        Array.fold
            (fun (list, current) (line: string) ->
                match Seq.tryHead line, current with
                | None, Some current -> Array.append list [| current |], None
                | Some '>', Some previous -> Array.append list [| previous |], Some { id = line[1..]; body = "" }
                | Some '>', None -> list, Some { id = line[1..]; body = "" }
                | Some c, Some current -> list, Some { current with body = current.body + line }
                // shouldn't happen, really
                | None, None -> list, None
                | Some _c, None -> list, None)
            ([||], None)
            (lines)

    match output with
    | ret, Some remainder -> Array.append ret [| remainder |]
    | ret, None -> ret


let tests =
    testList
        "fib"
        [ test "sample" {
              let output = 19L

              Expect.equal output 19L ""
          } ]

let main = runTestsWithCLIArgs [] [||] tests

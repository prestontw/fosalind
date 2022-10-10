#load "Common.fsx"
#r "nuget: Expecto"

open Common
open Expecto


let problem (input: string) =
    let [| searchString; substring |] | [| searchString; substring; _ |] =
        input.Split "\n"

    let indexOf (super: string) (sub: string) (start: int) =
        let r = super.IndexOf(sub, start)

        match r with
        | -1 -> None
        | r -> Some r

    let rec recurrences (found: int list) (super: string) (sub: string) (start: int) =
        if start > super.Length then
            found
        else
            let next = indexOf super sub start

            match next with
            | None -> found
            | Some i -> recurrences (i :: found) super sub (i + 1)

    let backwards_indexes = recurrences [] searchString substring 0

    let indexes =
        backwards_indexes
        |> List.rev
        |> List.map (fun x -> x + 1)
        |> List.map toString

    String.concat " " indexes


let tests =
    testList
        "fib"
        [ test "sample" {
              let output =
                  problem
                      """GATATATGCATATACTT
ATAT"""

              Expect.equal output "2 4 10" ""
          } ]

let main = runTestsWithCLIArgs [] [||] tests

#load "Common.fsx"
#r "nuget: Expecto"

open Common
open Expecto


let rec fib (n: int64) (k: int64) : int64 =
    match n with
    | 0L
    | 1L
    | 2L -> 1
    | h -> k * fib (h - 2L) k + fib (h - 1L) k


let tests =
    testList
        "fib"
        [ test "sample" {
              let output = fib 5L 3L

              Expect.equal output 19L ""
          } ]

let main = runTestsWithCLIArgs [] [||] tests

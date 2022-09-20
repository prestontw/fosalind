type Nucleobase =
    | A
    | C
    | G
    | T

let NucleobaseOfChar =
    function
    | 'A'
    | 'a' -> Some A
    | 'C'
    | 'c' -> Some C
    | 'G'
    | 'g' -> Some G
    | 'T'
    | 't' -> Some T
    | _ -> None

let readData fileName =
    System.IO.File.ReadLines("data/" + fileName)
    |> Seq.head

module RNA =
    type Ribonucleobase =
        | A
        | C
        | G
        | U

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

let tee x =
    printfn "%A" x
    x

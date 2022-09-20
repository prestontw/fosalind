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
    System.IO.File.ReadAllText("data/" + fileName)

let readFirstLine filename : string =
    filename
    |> readData
    |> fun s -> s.Split "\n"
    |> Seq.head

module Proteins =
    type AminoAcid =
        | A
        | C
        | D
        | E
        | F
        | G
        | H
        | I
        | K
        | L
        | M
        | N
        | P
        | Q
        | R
        | S
        | T
        | V
        | W
        | Y
        | Stop


module RNA =
    type Ribonucleobase =
        | A
        | C
        | G
        | U

    let riboBaseOfChar =
        function
        | 'A'
        | 'a' -> Some A
        | 'C'
        | 'c' -> Some A
        | 'G'
        | 'g' -> Some A
        | 'U'
        | 'u' -> Some A
        | _ -> None

    let encode a b c =
        match a, b, c with
        | U, U, U -> Proteins.F
        | U, U, C -> Proteins.F
        | U, U, A -> Proteins.L
        | U, U, G -> Proteins.L
        | U, C, U -> Proteins.S
        | U, C, C -> Proteins.S
        | U, C, A -> Proteins.S
        | U, C, G -> Proteins.S
        | U, A, U -> Proteins.Y
        | U, A, C -> Proteins.Y
        | U, A, A -> Proteins.Stop
        | U, A, G -> Proteins.Stop
        | U, G, U -> Proteins.C
        | U, G, C -> Proteins.C
        | U, G, A -> Proteins.Stop
        | U, G, G -> Proteins.W
        | C, U, U -> Proteins.L
        | C, U, C -> Proteins.L
        | C, U, A -> Proteins.L
        | C, U, G -> Proteins.L
        | C, C, U -> Proteins.P
        | C, C, C -> Proteins.P
        | C, C, A -> Proteins.P
        | C, C, G -> Proteins.P
        | C, A, U -> Proteins.H
        | C, A, C -> Proteins.H
        | C, A, A -> Proteins.Q
        | C, A, G -> Proteins.Q
        | C, G, U -> Proteins.R
        | C, G, C -> Proteins.R
        | C, G, A -> Proteins.R
        | C, G, G -> Proteins.R
        | A, U, U -> Proteins.I
        | A, U, C -> Proteins.I
        | A, U, A -> Proteins.I
        | A, U, G -> Proteins.M
        | A, C, U -> Proteins.T
        | A, C, C -> Proteins.T
        | A, C, A -> Proteins.T
        | A, C, G -> Proteins.T
        | A, A, U -> Proteins.N
        | A, A, C -> Proteins.N
        | A, A, A -> Proteins.K
        | A, A, G -> Proteins.K
        | A, G, U -> Proteins.S
        | A, G, C -> Proteins.S
        | A, G, A -> Proteins.R
        | A, G, G -> Proteins.R
        | G, U, U -> Proteins.V
        | G, U, C -> Proteins.V
        | G, U, A -> Proteins.V
        | G, U, G -> Proteins.V
        | G, C, U -> Proteins.A
        | G, C, C -> Proteins.A
        | G, C, A -> Proteins.A
        | G, C, G -> Proteins.A
        | G, A, U -> Proteins.D
        | G, A, C -> Proteins.D
        | G, A, A -> Proteins.E
        | G, A, G -> Proteins.E
        | G, G, U -> Proteins.G
        | G, G, C -> Proteins.G
        | G, G, A -> Proteins.G
        | G, G, G -> Proteins.G


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

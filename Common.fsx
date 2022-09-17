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

module RNA =
    type Ribonucleobase =
        | A
        | C
        | G
        | U

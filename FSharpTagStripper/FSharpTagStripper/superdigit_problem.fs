
module SuperdigitProblem 
open System

let concentrate sequence =
    let compute_digit prev_char next_char =
        match prev_char with
        | '0' -> (function |  _  ->  next_char) next_char
        | '1' -> (function | '0' -> '1' | '1' -> '2' | '2' -> '3' | '3' -> '4' | '4' -> '5' | '5' -> '6' | '6' -> '7' | '7' -> '8' | '8' -> '9' | '9' -> '1') next_char 
        | '2' -> (function | '0' -> '2' | '1' -> '3' | '2' -> '4' | '3' -> '5' | '4' -> '6' | '5' -> '7' | '6' -> '8' | '7' -> '9' | '8' -> '1' | '9' -> '2') next_char 
        | '3' -> (function | '0' -> '3' | '1' -> '4' | '2' -> '5' | '3' -> '6' | '4' -> '7' | '5' -> '8' | '6' -> '9' | '7' -> '1' | '8' -> '2' | '9' -> '3') next_char
        | '4' -> (function | '0' -> '4' | '1' -> '5' | '2' -> '6' | '3' -> '7' | '4' -> '8' | '5' -> '9' | '6' -> '1' | '7' -> '2' | '8' -> '3' | '9' -> '4') next_char
        | '5' -> (function | '0' -> '5' | '1' -> '6' | '2' -> '7' | '3' -> '8' | '4' -> '9' | '5' -> '1' | '6' -> '2' | '7' -> '3' | '8' -> '4' | '9' -> '5') next_char
        | '6' -> (function | '0' -> '6' | '1' -> '7' | '2' -> '8' | '3' -> '9' | '4' -> '1' | '5' -> '2' | '6' -> '3' | '7' -> '4' | '8' -> '5' | '9' -> '6') next_char
        | '7' -> (function | '0' -> '7' | '1' -> '8' | '2' -> '9' | '3' -> '1' | '4' -> '2' | '5' -> '3' | '6' -> '4' | '7' -> '5' | '8' -> '6' | '9' -> '7') next_char
        | '8' -> (function | '0' -> '8' | '1' -> '9' | '2' -> '1' | '3' -> '2' | '4' -> '3' | '5' -> '4' | '6' -> '5' | '7' -> '6' | '8' -> '7' | '9' -> '8') next_char
        | '9' -> (function | '0' -> '9' | '1' -> '1' | '2' -> '2' | '3' -> '3' | '4' -> '4' | '5' -> '5' | '6' -> '6' | '7' -> '7' | '8' -> '8' | '9' -> '9') next_char

    Seq.reduce (compute_digit) sequence

let test = concentrate ['8';'4';'9';'7';'3';'6';'3';'7';'4';]

let solve_problem () =
    let eofs = [-1;13;32] // end of file, enter-key, space
    let numeric_for_char_0 = 48
    let summation =  eofs |> IOHelpers.stdin_as_char_seq |> concentrate 
    let multiplier = eofs |> IOHelpers.stdin_as_char_seq |> Seq.toArray |> String |> Convert.ToInt32
    let total = multiplier * ((int summation) - numeric_for_char_0)
    let result = concentrate (total.ToString().ToCharArray())

    Operators.stdout.Write(result)
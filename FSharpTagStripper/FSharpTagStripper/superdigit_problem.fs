
module superdigit_problem 
    open System

    let concentrate sequence =
        let compute_digit prev_char next_char =
            match prev_char with
            | '0' -> next_char
            | '1' -> match next_char with | '0' -> '1' | '1' -> '2' | '2' -> '3' | '3' -> '4' | '4' -> '5' | '5' -> '6' | '6' -> '7' | '7' -> '8' | '8' -> '9' | '9' -> '1' 
            | '2' -> match next_char with | '0' -> '2' | '1' -> '3' | '2' -> '4' | '3' -> '5' | '4' -> '6' | '5' -> '7' | '6' -> '8' | '7' -> '9' | '8' -> '1' | '9' -> '2' 
            | '3' -> match next_char with | '0' -> '3' | '1' -> '4' | '2' -> '5' | '3' -> '6' | '4' -> '7' | '5' -> '8' | '6' -> '9' | '7' -> '1' | '8' -> '2' | '9' -> '3' 
            | '4' -> match next_char with | '0' -> '4' | '1' -> '5' | '2' -> '6' | '3' -> '7' | '4' -> '8' | '5' -> '9' | '6' -> '1' | '7' -> '2' | '8' -> '3' | '9' -> '4' 
            | '5' -> match next_char with | '0' -> '5' | '1' -> '6' | '2' -> '7' | '3' -> '8' | '4' -> '9' | '5' -> '1' | '6' -> '2' | '7' -> '3' | '8' -> '4' | '9' -> '5' 
            | '6' -> match next_char with | '0' -> '6' | '1' -> '7' | '2' -> '8' | '3' -> '9' | '4' -> '1' | '5' -> '2' | '6' -> '3' | '7' -> '4' | '8' -> '5' | '9' -> '6' 
            | '7' -> match next_char with | '0' -> '7' | '1' -> '8' | '2' -> '9' | '3' -> '1' | '4' -> '2' | '5' -> '3' | '6' -> '4' | '7' -> '5' | '8' -> '6' | '9' -> '7' 
            | '8' -> match next_char with | '0' -> '8' | '1' -> '9' | '2' -> '1' | '3' -> '2' | '4' -> '3' | '5' -> '4' | '6' -> '5' | '7' -> '6' | '8' -> '7' | '9' -> '8' 
            | '9' -> match next_char with | '0' -> '9' | '1' -> '1' | '2' -> '2' | '3' -> '3' | '4' -> '4' | '5' -> '5' | '6' -> '6' | '7' -> '7' | '8' -> '8' | '9' -> '9' 

        Seq.reduce (compute_digit) sequence

    let test = concentrate ['8';'4';'9';'7';'3';'6';'3';'7';'4';]

    let solve_problem () =
        let get_char () =
            let code = Operators.stdin.Read()
            match code with
            | 32 -> None
            | 13 -> None
            | -1 -> None
            |  _ -> Some(char code, 0)

        let input () 
            = (Seq.unfold (fun _ -> get_char()) 0) 

        let summation = concentrate (input())

        let multiplier = input() |> Seq.toArray |> String |> Convert.ToInt32

        let total = multiplier * ((int summation)-48)

        let result = concentrate (total.ToString().ToCharArray())

        Operators.stdout.Write(result)

    //solve_problem()

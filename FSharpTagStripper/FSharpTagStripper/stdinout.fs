
module IOHelpers

open System

    let stdin_as_char_seq chars = 
        let get_char chars =
            let code = Operators.stdin.Read()
            match chars with
            | _    when Seq.exists (fun x -> x = code) chars -> None
            | _ -> Some(char code, 0)

        (Seq.unfold (fun _ -> get_char chars) 0) 

// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

module program =

    open System

    [<EntryPoint>]
    let main argv = 
    
        // Motivated by an interest in simplifying an earlier C# FSM design.


        let result = HtmlHelpers.strip_tags "This text is HTML with an embedded<img> image tag."

        // Motivated by: https://social.msdn.microsoft.com/Forums/vstudio/en-US/11be467d-9153-4318-8d79-f300ce9ccf39/f-get-each-element-from-the-list-in-seqchoose?forum=fsharpgeneral

        let lines = [| ("A 1 100"); ("B 2 200"); ("C 3 300") |]

        let tupelize input =
            let create_tuple (text:string) =
                (Int32.Parse (text.Split ' ').[1], Int32.Parse (text.Split ' ').[2])
            Array.map create_tuple input
    
        let tuples = tupelize lines 

        // Drop every n'the element: http://ocaml.org/learn/tutorials/99problems.html

        let rec drop_every n sequence = 
            let has_at_least j sequence = Seq.truncate j sequence |> Seq.length >= j
            match has_at_least n sequence with
            | false -> sequence
            | true  -> Seq.append (Seq.take (n-1) sequence) (drop_every n (Seq.skip n sequence))
           
        let ss = drop_every 3 "123456789" |> Seq.toList   // list just makes viewing results in debug easier.
    
        // Rotate a list n places left or right: http://ocaml.org/learn/tutorials/99problems.html
        
        let k = (1,2)


        let tst n data = data |> ListHelpers.t 1 |> ListHelpers.s (n-2) |> ListHelpers.t 1

        let rec terms c n data = 
            match c with
            | 3 -> tst n data
            | _ -> tst n data |> terms (c-1) n

        let ring M N list =
            let tuple = ([],list,[])
            tuple |> ListHelpers.t N |> terms N N |> ListHelpers.t N

        let tbt = ['a';'b';'c';'d';'e';'f';'g';'h';'i']
        let fbf = ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p']
        let pbp = ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y']
        
        let mtbt = (5,5,pbp) (* rows, cols, list*)

        let row M matrix = 
            (ListHelpers.third matrix) |> Seq.skip (M * (ListHelpers.first matrix)) |> Seq.take (ListHelpers.second matrix)

        let col N matrix = 
            seq {for r in 0..(-1 + ListHelpers.first matrix) -> row r matrix |> Seq.skip N |> Seq.head}
        
        let r1 = row 1 mtbt |> Seq.toList
        let c2 = col 2 mtbt |> Seq.toList

        // returns the specified 'ring' of elements from a matrix in clockwise order.
        let ring n matrix =
            let rows = ListHelpers.first matrix - 1
            let cols = ListHelpers.second matrix - 1
            let coltake = cols - (2 * n)
            let rowtake = rows - (2 * n)
            let upper = (row n matrix        |> Seq.skip n |> Seq.take (coltake)) |> Seq.toList
            let right = (col (cols-n) matrix |> Seq.skip n |> Seq.take (rowtake))  |> Seq.toList
            let lower = (row (rows-n) matrix |> Seq.rev    |> Seq.skip n |> Seq.take (coltake))  |> Seq.toList
            let left  = (col n matrix        |> Seq.rev    |> Seq.skip n |> Seq.take (rowtake))  |> Seq.toList

            seq {yield! upper;
                 yield! right;
                 yield! lower;
                 yield! left}

        let otbt = ring 2 mtbt |> Seq.toList
//        let ofbf = ring 4 4 fbf  
//        let opbp = ring 5 5 pbp
//        let rotbt = ListHelpers.first otbt |> ListHelpers.rotate 1

        let rotated = "abcdefghij" |> ListHelpers.rotate -2 

        SuperdigitProblem.solve_problem()

        0 // return an integer exit code
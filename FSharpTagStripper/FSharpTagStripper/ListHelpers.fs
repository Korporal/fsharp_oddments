module ListHelpers

    let rotate n list =  
        let rec rotate_list n list =
            let take_first list = list |> List.take 1
            let skip_first list = list |> List.skip 1
            let take_last  list = List.rev list |> take_first
            let skip_last  list = List.rev list |> skip_first |> List.rev
            let append_to lista listb = List.append lista listb
            match n, list with
            | _, [] -> []
            | _, [ _ ] -> list
            | _, _  when n = 0 -> list
            | _, _  when n % List.length list = 0 -> list
            | _, _  when n > 0 -> (list |> take_first |> append_to (list |> skip_first)) |> rotate_list (n - 1) 
            | _, _  when n < 0 -> (list |> skip_last  |> append_to (list |> take_last))  |> rotate_list (n + 1) 
        rotate_list n (Seq.toList list)

    let first  (x,_,_) = x
    let second (_,x,_) = x
    let third  (_,_,x) = x

    // Consumes a tuple of two lists and returns a tuple where the first list is unchanged but the second list has n items removed.
    let s n data = (first data, (List.skip n (second data)), (third data @ List.take n (second data)))            

    // Consumes a tuple of two lists and returns a tuple where the first list has n items appended from the second list 
    // and the second list has those same items removed.
    let t n data = (first data @ (List.take n (second data)), (second data |> List.skip n), (third data)) 
    

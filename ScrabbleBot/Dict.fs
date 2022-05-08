module internal Dictionary

    open System.Collections.Generic  
    
    type Dict = 
        | Leaf of bool
        | Node of bool * Dictionary<char, Dict>
    
    let empty () = Leaf false
    
    let rec insert (s:string)  =
        function
        | Leaf _ when s.Length = 0 -> Leaf true
        | Node (_, d) when s.Length = 0 -> Node (true, d)
        | Leaf b when s.Length > 0 ->
          let newDictionary : Dictionary<char, Dict> = Dictionary<char, Dict>()
          let substring = s.[1..]
          newDictionary.Add(s.[0], insert substring (empty()))
          Node(b, newDictionary)
        | Node (b, d) when s.Length > 0 ->
          let substring = s.[1..]
          match d.TryGetValue s.[0] with
          | (false, _) ->
            d.Add(s.[0], insert substring (empty()))
            Node(b, d)
          | (true, node) ->
            d.Add(s.[0], insert substring node)
            Node(b, d)
        | Leaf(_) -> failwith "Not Implemented in Dictionary Insert"
        | Node(_, _) -> failwith "Not Implemented in Dictionary Insert"
          
    let step (c: char) (dict) =
        match dict with
        | Leaf _ -> None
        | Node(_, subdict) ->
            match subdict.TryGetValue(c) with
            | (false, _) -> None
            | (true, node) ->
               match node with
               | Leaf b -> Some (b, node) 
               | Node (b, _) -> Some(b, node)
               
    let rec lookup (s:string) (dict) =
        match (step s.[0] dict) with
        | None -> false
        | Some (b, subdict) when s.Length = 1  -> b
        | Some (b, subdict) ->
            lookup s.[1..] subdict
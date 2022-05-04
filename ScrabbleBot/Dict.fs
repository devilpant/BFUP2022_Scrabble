//Alle typer skal være internal
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
        | Node (b, d) when s.Length > 0 -> if d.TryGetValue(s.[0]) then insert substring d(s.[0]) else
          let newDictionary : Dictionary<char, Dict> = Dictionary<char, Dict>()
          let substring = s.[1..]
          d.Add(s.[0], insert substring (empty()))
          Node(b, newDictionary)
    
            
    //let lookup (s:string) (D x) = List.exists ((=)s) x
    //let step (c: char) (D dict) = function
         
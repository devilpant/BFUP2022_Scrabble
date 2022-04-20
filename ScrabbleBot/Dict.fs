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
          Node(b, newDictionary.Add(s.[0], insert s.tail empty) )
        
        | Node (b, d) when 
    
    //let lookup (s:string) (D x) = List.exists ((=)s) x
    //let step (c: char) (D dict) = function
         
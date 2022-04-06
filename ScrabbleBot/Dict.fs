module internal Dictionary
    
    type Dict = 
        | D of List<string>
    
    let empty () = D List.empty
    let insert (s:string) (D x) = D (s :: x)
    let lookup (s:string) (D x) = List.exists ((=)s) x
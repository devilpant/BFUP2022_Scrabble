module internal StateMonad

    type Error = 
        | VarExists of string
        | VarNotFound of string
        | IndexOutOfBounds of int
        | DivisionByZero 
        | ReservedName of string           

    type Result<'a, 'b>  =
        | Success of 'a
        | Failure of 'b

    type State = { vars     : Map<string, int> list
                   word     : (char * int) list 
                   reserved : Set<string> }

    type SM<'a> = S of (State -> Result<'a * State, Error>)

    let mkState lst word reserved = 
           { vars = [Map.ofList lst];
             word = word;
             reserved = Set.ofList reserved }

    let evalSM (s : State) (S a : SM<'a>) : Result<'a, Error> =
        match a s with
        | Success (result, _) -> Success result
        | Failure error -> Failure error

    let bind (f : 'a -> SM<'b>) (S a : SM<'a>) : SM<'b> =
        S (fun s ->
              match a s with
              | Success (b, s') -> 
                match f b with 
                | S g -> g s'
              | Failure err     -> Failure err)

    let ret (v : 'a) : SM<'a> = S (fun s -> Success (v, s))
    let fail err     : SM<'a> = S (fun s -> Failure err)

    let (>>=)  x f = bind f x
    let (>>>=) x f = x >>= (fun () -> f)

    let push : SM<unit> = 
        S (fun s -> Success ((), {s with vars = Map.empty :: s.vars}))

    let pop : SM<unit> = S (fun s -> Success ((), {s with vars = s.vars.Tail}))       

    let wordLength : SM<int> = S (fun s -> Success (s.word.Length, s))      

    let characterValue (pos : int) : SM<char> = S (fun s ->
        match pos with 
        | pos when pos >= List.length s.word -> Failure(IndexOutOfBounds pos)
        | pos when pos < 0 -> Failure(IndexOutOfBounds pos)
        | pos ->  Success (fst(s.word.[pos]), s))    

    let pointValue (pos : int) : SM<int> = S (fun s ->
        match pos with 
        | pos when pos >= List.length s.word -> Failure(IndexOutOfBounds pos)
        | pos when pos < 0 -> Failure(IndexOutOfBounds pos)
        | pos ->  Success (snd(s.word.[pos]), s))     

    let lookup (x : string) : SM<int> = 
        let rec aux =
            function
            | []      -> None
            | m :: ms -> 
                match Map.tryFind x m with
                | Some v -> Some v
                | None   -> aux ms

        S (fun s -> 
              match aux (s.vars) with
              | Some v -> Success (v, s)
              | None   -> Failure (VarNotFound x))

    let declare (var : string) : SM<unit> = 
        
        let add =
            function
            | [] -> []
            | m :: ms -> (Map.add var 0 m) :: ms

        let aux =
            function
            | [] -> None
            | m :: ms ->
                match Map.tryFind var m with
                | Some _ -> Some VarExists
                | None -> None

        let aux2 (set : Set<string>) (list : list<Map<string, int>>) = 
            match Set.contains var set with
            | true -> Some ReservedName 
            | false -> aux list
      
        S (fun s -> 
            match aux2 (s.reserved) (s.vars) with
            | Some v -> Failure (v var)
            | None -> Success ((), {s with vars = add s.vars}))
    
    let update (var : string) (value : int) : SM<unit> = 
        
        let updateMap m = m |> Map.add var value

        let rec aux =
            function
            | [] -> None
            | m :: ms ->
                match Map.tryFind var m with
                | Some _ -> Some m
                | None -> aux ms
        
        let order oldMap newMap mapList = 
            let rec replaceC oldMap newMap mapList c = 
                match mapList with
                | [] -> c []
                | x :: xs when x = oldMap -> c (newMap :: xs)
                | x :: xs -> replaceC oldMap newMap xs (fun r -> x :: r)
            replaceC oldMap newMap mapList id

        S (fun s ->
            match aux s.vars with 
            | Some m -> 
                let m' = updateMap m
                Success ((), {s with vars = (order m m' s.vars)})
            | None -> Failure (VarNotFound var))     
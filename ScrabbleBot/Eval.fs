﻿// Insert your updated Eval.fs file here from Assignment 7. All modules must be internal.

module internal Eval

    open StateMonad

    let hello = [('H', 4); ('E', 1); ('L', 1); ('L', 1); ('O', 1)]
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []
        
    let add a b = 
            a >>= fun x ->
            b >>= fun y ->
            ret (x + y)

    let div a b = 
            a >>= fun x ->
            b >>= fun y ->
            if y <> 0 then ret (x / y) else fail (DivisionByZero)   

    let isVowel = 
            function
            | 'A' | 'E' | 'I' | 'O' | 'U' | 'Y' | 'a' | 'e' | 'i' | 'o' | 'u' | 'y' -> true
            | _ -> false

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
        | C of char (* Character value *)
        | CV of aExp (* Character lookup at word index *)
        | ToUpper of cExp
        | ToLower of cExp
        | IntToChar of aExp

    type bExp =
        | TT (* true *)
        | FF (* false *)

        | AEq of aExp * aExp (* numeric equality *)
        | ALt of aExp * aExp (* numeric less than *)

        | Not of bExp (* boolean not *)
        | Conj of bExp * bExp (* boolean conjunction *)

        | IsVowel of cExp (* check for vowel *)
        | IsLetter of cExp (* check for letter *)
        | IsDigit of cExp (* check for digit *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
        
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    let rec arithEval a : SM<int> = 
        match a with
        | N x -> ret x
        | V x -> lookup x
        | WL -> wordLength
        | PV x -> (arithEval x) >>= fun y -> pointValue y
        | Add (x, y) -> add (arithEval x) (arithEval y)
        | Sub (x, y) -> (arithEval x) >>= fun m ->
                        (arithEval y) >>= fun n ->
                        ret (m - n)
        | Mul (x, y) -> (arithEval x) >>= fun m ->
                        (arithEval y) >>= fun n ->
                        ret (m * n)
        | Div (x, y) -> div (arithEval x) (arithEval y)
        | Mod (x, y) -> (arithEval x) >>= fun m ->
                        (arithEval y) >>= fun n ->
                        if n <> 0 then ret (m % n) else fail (DivisionByZero) 
        | CharToInt x -> charEval x >>= fun y -> ret (int y)  

    and charEval c : SM<char> = 
        match c with
        | C x -> ret x
        | CV x -> (arithEval x) >>= fun y -> characterValue y
        | ToUpper x -> (charEval x) >>= fun y -> ret (System.Char.ToUpper y)
        | ToLower x -> (charEval x) >>= fun y -> ret (System.Char.ToLower y)
        | IntToChar x -> (arithEval x) >>= fun y -> ret (char y)

    let rec boolEval b : SM<bool> = 
        match b with
        | TT -> ret true              
        | FF -> ret false          
        | AEq (x, y) -> (arithEval x) >>= fun m ->
                        (arithEval y) >>= fun n ->
                        ret (m = n)
        | ALt (x, y) -> (arithEval x) >>= fun m ->
                        (arithEval y) >>= fun n ->
                        ret (m < n)
        | Not x -> (boolEval x) >>= fun y -> ret (not y)
        | Conj (x, y) -> (boolEval x) >>= fun m ->
                        (boolEval y) >>= fun n ->
                        ret (m && n)
        | IsVowel x -> (charEval x) >>= fun y -> ret (isVowel y)
        | IsLetter x -> (charEval x) >>= fun y -> ret (System.Char.IsLetter y)
        | IsDigit x -> (charEval x) >>= fun y -> ret (System.Char.IsDigit y)

    type stm = (* statements *)
        | Declare of string (* variable declaration *)
        | Ass of string * aExp (* variable assignment *)
        | Skip (* nop *)
        | Seq of stm * stm (* sequential composition *)
        | ITE of bExp * stm * stm (* if-then-else statement *)
        | While of bExp * stm (* while statement *)

    let rec stmntEval stmnt : SM<unit> = failwith "Not implemented"

    type StateBuilder() =

        member this.Bind(f, x) = f >>= x
        member this.Return(x) = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f) = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let prog = new StateBuilder()

    let arithEval2 a = failwith "Not implemented"
    let charEval2 c = failwith "Not implemented"
    let rec boolEval2 b = failwith "Not implemented"

    let stmntEval2 stm = failwith "Not implemented"

    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>

    let stmntToSquareFun stm = failwith "Not implemented"


    type coord = int * int

    type boardFun = coord -> Result<squareFun option, Error>

    let stmntToBoardFun stm m = failwith "Not implemented"

    type board = { 
        center: coord
        defaultSquare: squareFun
        squares: boardFun 
    }

    let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"

// Insert your updated Eval.fs file here from Assignment 7. All modules must be internal.

module internal Eval

open StateMonad

let hello =
    [ ('H', 4)
      ('E', 1)
      ('L', 1)
      ('L', 1)
      ('O', 1) ]

let state = mkState [ ("x", 5); ("y", 42) ] hello [ "_pos_"; "_result_" ]
let emptyState = mkState [] [] []

let isVowel c =
    let upperC = System.Char.ToUpper(c)

    (upperC = 'A'
     || upperC = 'E'
     || upperC = 'O'
     || upperC = 'I'
     || upperC = 'U')


let add a b =
    a >>= fun x -> b >>= fun y -> ret (x + y)

let sub a b =
    a >>= fun x -> b >>= fun y -> ret (x - y)

let mul a b =
    a >>= fun x -> b >>= fun y -> ret (x * y)

let modu a b =
    a
    >>= fun x ->
            b
            >>= fun y ->
                    if y <> 0 then
                        ret (x % y)
                    else
                        fail DivisionByZero

let div a b =
    a
    >>= fun x ->
            b
            >>= fun y ->
                    if y <> 0 then
                        ret (x / y)
                    else
                        fail DivisionByZero

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

let (.+.) a b = Add(a, b)
let (.-.) a b = Sub(a, b)
let (.*.) a b = Mul(a, b)
let (./.) a b = Div(a, b)
let (.%.) a b = Mod(a, b)

let (~~) b = Not b
let (.&&.) b1 b2 = Conj(b1, b2)

let (.||.) b1 b2 =
    ~~(~~b1 .&&. ~~b2) (* boolean disjunction *)

let (.->.) b1 b2 =
    (~~b1) .||. b2 (* boolean implication *)

let (.=.) a b = AEq(a, b)
let (.<.) a b = ALt(a, b)
let (.<>.) a b = ~~(a .=. b)
let (.<=.) a b = a .<. b .||. ~~(a .<>. b)

let (.>=.) a b =
    ~~(a .<. b) (* numeric greater than or equal to *)

let (.>.) a b =
    ~~(a .=. b)
    .&&. (a .>=. b) (* numeric greater than *)

let rec arithEval a : SM<int> =
    match a with
    | N n -> ret n
    | V v -> lookup v
    | WL -> wordLength
    | PV x -> arithEval x >>= pointValue
    | Add (x, y) -> add (arithEval x) (arithEval y)
    | Sub (x, y) -> sub (arithEval x) (arithEval y)
    | Mul (x, y) -> mul (arithEval x) (arithEval y)
    | Div (x, y) -> div (arithEval x) (arithEval y)
    | Mod (x, y) -> modu (arithEval x) (arithEval y)
    | CharToInt c -> charEval c >>= (fun c -> ret (int c))

and charEval c : SM<char> =
    match c with
    | C char -> ret char
    | ToUpper x ->
        charEval x
        >>= (fun c -> ret (System.Char.ToUpper c))
    | ToLower x ->
        charEval x
        >>= (fun c -> ret (System.Char.ToLower c))
    | CV cv -> arithEval cv >>= characterValue
    | IntToChar n -> arithEval n >>= (fun n -> ret (char n))

let rec boolEval b : SM<bool> =
    match b with
    | TT -> ret true
    | FF -> ret false
    | AEq (a, b) ->
        (arithEval a)
        >>= fun x -> (arithEval b) >>= fun y -> ret (x = y)
    | ALt (a, b) ->
        (arithEval a)
        >>= fun x -> (arithEval b) >>= fun y -> ret (x < y)
    | Not x -> boolEval x >>= (fun x -> ret (not x))
    | Conj (a, b) ->
        (boolEval a)
        >>= fun x -> (boolEval b) >>= fun y -> ret (x && y)
    | IsDigit x ->
        charEval x
        >>= (fun c -> ret (System.Char.IsDigit c))
    | IsLetter x ->
        charEval x
        >>= (fun c -> ret (System.Char.IsLetter c))
    | IsVowel x -> charEval x >>= (fun c -> ret (isVowel c))



type stm = (* statements *)
    | Declare of string (* variable declaration *)
    | Ass of string * aExp (* variable assignment *)
    | Skip (* nop *)
    | Seq of stm * stm (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm (* while statement *)

let rec stmntEval stmnt : SM<unit> = failwith "Not implemented"

(* Part 3 (Optional) *)

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

(* Part 4 *)

type word = (char * int) list
type squareFun = word -> int -> int -> Result<int, Error>

let stmntToSquareFun stm = failwith "Not implemented"


type coord = int * int

type boardFun = coord -> Result<squareFun option, Error>

let stmntToBoardFun stm m = failwith "Not implemented"

type board =
    { center: coord
      defaultSquare: squareFun
      squares: boardFun }

let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"

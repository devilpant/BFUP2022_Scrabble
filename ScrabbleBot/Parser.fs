// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

open ScrabbleUtil // NEW. KEEP THIS LINE.
open System
open Eval
open FParsecLight.TextParser // Industrial parser-combinator library. Use for Scrabble Project.

let pIntToChar = pstring "intToChar"
let pPointValue = pstring "pointValue"

let pCharToInt = pstring "charToInt"
let pToUpper = pstring "toUpper"
let pToLower = pstring "toLower"
let pCharValue = pstring "charValue"

let pTrue = pstring "true"
let pFalse = pstring "false"
let pIsDigit = pstring "isDigit"
let pIsLetter = pstring "isLetter"
let pIsVowel = pstring "isVowel"

let pif = pstring "if"
let pthen = pstring "then"
let pelse = pstring "else"
let pwhile = pstring "while"
let pdo = pstring "do"
let pdeclare = pstring "declare"

let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
let pletter = satisfy System.Char.IsLetter <?> "letter"

let palphanumeric =
    satisfy System.Char.IsLetterOrDigit
    <?> "alphanumeric"

let spaces = many whitespaceChar <?> "spaces"
let spaces1 = many1 whitespaceChar <?> "space1"

let (.>*>.) p1 p2 = p1 .>> spaces .>>. p2
let (.>*>) p1 p2 = p1 .>> spaces .>> p2
let (>*>.) p1 p2 = p1 .>> spaces >>. p2

let parenthesise p =
    between (pchar '(') (pchar ')') (spaces >>. p .>> spaces)

let angleParenthesise p =
    between (pchar '{') (pchar '}') (spaces >>. p .>> spaces)

let pid =
    let aux (list: char list) = System.String.Concat(list)

    pletter <|> pchar ('_')
    .>>. many (palphanumeric <|> pchar ('_'))
    |>> fun (c, cl) -> aux (c :: cl)


let unop op = (>*>.) op
let binop op p1 p2 = p1 .>*>. unop op p2

let TermParse, tref = createParserForwardedToRef<aExp> ()
let ProdParse, pref = createParserForwardedToRef<aExp> ()
let AtomParse, aref = createParserForwardedToRef<aExp> ()
let CharParse, cref = createParserForwardedToRef<cExp> ()

let AexpParse = TermParse
let CexpParse = CharParse
let BexpParse = pstring "not implemented"
let stmParse = pstring "not implemented"

let AddParse =
    binop (pchar '+') ProdParse TermParse |>> Add
    <?> "Add"

let SubParse =
    binop (pchar '-') ProdParse TermParse |>> Sub
    <?> "Sub"

do tref := choice [ AddParse; SubParse; ProdParse ]

let MulParse =
    binop (pchar '*') AtomParse ProdParse |>> Mul
    <?> "Mul"

let DivParse =
    binop (pchar '/') AtomParse ProdParse |>> Div
    <?> "Div"

let ModParse =
    binop (pchar '%') AtomParse ProdParse |>> Mod
    <?> "Mod"

do
    pref
    := choice [ MulParse
                DivParse
                ModParse
                AtomParse ]

let NegParse =
    unop (pchar '-') pint32
    |>> (fun n -> Mul(N -1, N n))
    <?> "Neg"

let NParse = pint32 |>> N <?> "Int"
let PVParse = unop pPointValue AtomParse |>> PV <?> "PV"
let VParse = pid |>> V <?> "Str"
let ParParse = parenthesise TermParse

let CharToIntParse =
    unop pCharToInt (parenthesise CexpParse)
    |>> CharToInt
    <?> "CharToInt"

let CParse =
    between (pchar ''') (pchar ''') (anyChar) |>> C
    <?> "Char"

let CVParse =
    unop pCharValue (parenthesise AexpParse) |>> CV
    <?> "CV"

let ToUpperParse =
    unop pToUpper (parenthesise CexpParse) |>> ToUpper
    <?> "ToUpper"

let ToLowerParse =
    unop pToLower (parenthesise CexpParse) |>> ToLower
    <?> "ToLower"

let IntToCharParse =
    unop pIntToChar (parenthesise AexpParse)
    |>> IntToChar
    <?> "IntToChar"

do
    aref
    := choice [ CharToIntParse
                NegParse
                NParse
                PVParse
                VParse
                ParParse ]

do
    cref
    := choice [ CVParse
                ToUpperParse
                ToLowerParse
                IntToCharParse
                CParse ]



(* The rest of your parser goes here *)

type word = (char * int) list
type square = Map<int, word -> int -> int -> int>

let parseSquareFun _ = failwith "not implemented"

let parseBoardFun _ = failwith "not implemented"

type boardFun = coord -> square option

type board =
    { center: coord
      defaultSquare: square
      squares: boardFun }

let parseBoardProg (bp: boardProg) : board = failwith "not implemented"

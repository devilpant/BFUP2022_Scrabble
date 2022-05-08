// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open StateMonad
    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open System
    open Eval
    open FParsecLight.TextParser // Industrial parser-combinator library. Use for Scrabble Project.

    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel    = pstring "isVowel"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy(System.Char.IsWhiteSpace) <?> "whitespace"
    let pletter        = satisfy(System.Char.IsLetter) <?> "letter"
    let palphanumeric  = satisfy(System.Char.IsLetterOrDigit) <?> "alphanumeric"

    let spaces  = many whitespaceChar <?> "spaces"
    let spaces1 = many1 whitespaceChar <?> "space1"

    let (.>*>.) p1 p2 = p1 .>> spaces .>>. p2 
    let (.>*>) p1 p2  = p1 .>> spaces .>> p2
    let (>*>.) p1 p2  = p1 >>. spaces >>. p2

    let parenthesise p = pchar '(' >*>. p .>*> pchar ')'
    let curlyparenthesise p = pchar '{' >*>. p .>*> pchar '}'

    let charListToStr (charL : char * char list) = fst charL :: snd charL |> List.toArray |> (fun x -> System.String.Concat(x))
    let pid = ((pletter <|> pchar '_') .>>. (many(palphanumeric <|> pchar '_'))) |>> charListToStr

    let unop op a = op >*>. a
    let binop op p1 p2 = (p1 .>*> op) .>*>. p2

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    let CharParse, cref = createParserForwardedToRef<cExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse;]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]

    let CTIParse = unop pCharToInt (parenthesise CharParse) |>> CharToInt <?> "CharToInt"
    let NegationParse = unop (pchar '-') pint32 |>> fun n -> Mul(N (-1), N n)
    let ParParse = parenthesise TermParse
    let PVParse = unop pPointValue ParParse |>> PV <?> "PV"
    let VarParse = pid |>> V <?> "V"
    let NParse   = pint32 |>> N <?> "Int"
    do aref := choice [CTIParse; NegationParse; PVParse; ParParse; VarParse; NParse;] 

    let CVParse = unop pCharValue ParParse |>> CV <?> "CV"
    let TLParse = unop pToLower (parenthesise CharParse) |>> ToLower <?> "ToLower"
    let TUParse = unop pToUpper (parenthesise CharParse) |>> ToUpper <?> "ToUpper"
    let ITCParse = unop pIntToChar ParParse |>> IntToChar <?> "IntToChar"
    let CParse = pchar ''' >>. anyChar .>> pchar ''' |>> C <?> "C" 
    do cref := choice [CVParse; TLParse; TUParse; ITCParse; CParse]

    let AexpParse = TermParse 

    let CexpParse = CharParse

    let BexpParse = pstring "not implemented"

    let stmntParse = pstring "not implemented"

    (* The rest of your parser goes here *)
    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>

    let parseSquareProg _ = failwith "not implemented"

    let parseBoardFun _ = failwith "not implemented"
    
    type boardFun2 = coord -> Result<square option, Error>
        
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }
    
    // Default (unusable) board in case you are not implementing a parser for the DSL.
    let mkBoard : boardProg -> board = fun _ -> {center = (0,0); defaultSquare = Map.empty; squares = fun _ -> Success (Some Map.empty)}
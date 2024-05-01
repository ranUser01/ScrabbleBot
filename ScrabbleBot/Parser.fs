// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open StateMonad
    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open Eval
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    
    let pIntToChar  = pstring "intToChar" 
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "True"
    let pFalse      = pstring "False"
    let pIsDigit    = pstring "IsDigit"
    let pIsLetter   = pstring "IsLetter"
    let pIsVowel    = pstring "IsVowel"

    let pif       = pstring "if" 
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy System.Char.IsLetter  <?> "letter"
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"

    let spaces         = many (pchar ' ') <?> "spaces" 
    let spaces1        = many1 (pchar ' ') <?> "space1" 

    let (.>*>.) p1 p2 = (p1 .>> spaces) .>>. p2
    let (.>*>) p1 p2  = (p1 .>> spaces) .>> p2
    let (>*>.) p1 p2  = (p1 .>> spaces) >>. p2

    let parenthesise p = ( pchar '(' ) .>> spaces >>. p .>> spaces .>> ( pchar ')' )

    let pid = 
        (pletter <|> pchar '_') 
        .>>. (many (palphanumeric <|> pchar '_')) 
        |>> fun (x, xs) -> 
            let xsAsString = System.String.Concat xs
            System.String.Concat(x, xsAsString)

    let unop (op : Parser<'a>) (a : Parser<'b>) = op >*>. spaces >*>. a

    let binop op a b = a .>*> op .>*>. b

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()

    //For compilation reasons
    let CevalParse, CexpRef = createParserForwardedToRef<cExp>()
    //let CParse = parenthesise (palphanumeric <|> ((pchar '\'') >>. palphanumeric .>> (pchar '\''))) |>> C <?> "C"
    let CParse = 
        (parenthesise (palphanumeric <|> ((pchar '\'') >>. anyChar .>> (pchar '\'')))) <|>
        (palphanumeric <|> ((pchar '\'') >>. anyChar .>> (pchar '\''))) 
        |>> C <?> "C"

    
    let ToUpperParse = unop pToUpper CevalParse |>> ToUpper <?> "toUpper"
    let ToLowerParse = unop pToLower CevalParse |>> ToLower <?> "toLower"
    let CParParse = parenthesise CevalParse
    let CexpParse = CevalParse

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [DivParse; MulParse; ModParse; AtomParse]

    let NParse   = pint32 |>> N <?> "Int"
    let VParse   = pid |>> V <?> "String"
    let PVParse = unop pPointValue AtomParse |>> PV <?> "PV"
    let NegParse = unop (pchar '-') AtomParse |>> (fun x -> Mul(N(-1), x)) <?> "Negation"
    let ParParse = parenthesise TermParse

    let CharToIntParse = unop pCharToInt CexpParse |>> CharToInt <?> "charToInt"

    do aref := choice [ParParse; NegParse; PVParse; NParse; CharToIntParse; VParse]

    let AexpParse = TermParse 
    
    let IntToCharParse = unop pIntToChar AexpParse |>> IntToChar <?> "intToChar "
    let CVParse = unop pCharValue AexpParse |>> CV <?> "CV"

    do CexpRef := choice [CParParse; CVParse; ToUpperParse; ToLowerParse; IntToCharParse; CParse]

    // From here I cannot copy from previous assigments

    let BexpParse = pstring "not implemented"

    let stmParse = pstring "not implemented"



    (* The rest of your parser goes here *)
    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>
    
    type boardFun2 = coord -> Result<square option, Error>
        
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }
    
    // Default (unusable) board in case you are not implementing a parser for the DSL.
    let mkBoard : boardProg -> board = fun _ -> {center = (0,0); defaultSquare = Map.empty; squares = fun _ -> Success (Some Map.empty)}

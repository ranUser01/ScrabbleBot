// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding. Done
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser. Done

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

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel   = pstring "isVowel"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy System.Char.IsLetter <?> "letter"
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"
    
    let pnumber = satisfy System.Char.IsDigit <?> "number"

    let spaces         = many whitespaceChar <?> "space"
    let spaces1        = many1 whitespaceChar <?> "space1"

    let (.>*>.) p1 p2 = p1 .>> spaces .>>. p2
    let (.>*>) p1 p2  = p1 .>> spaces .>> p2
    let (>*>.) p1 p2  = p1 .>> spaces >>. p2

    let parenthesise p = pchar '(' >*>. p .>*> pchar ')'
    
    let curlisise p = pchar '{' >*>. p .>*> pchar '}'
    
    let charcharToStr (c1:char, c2:char list) = System.String(List.append [c1] c2 |> Array.ofList)
       
    let charListToStr (c2:char list) = System.String(c2 |> Array.ofList)
    
    let pid = pletter <|> pchar '_' .>>. many (palphanumeric <|> pchar '_') |>> charcharToStr   
      
    let unop op p1 = op >*>. p1 
    let binop op p1 p2 = p1 .>*> op .>*>. p2

    let TermParse, tref = createParserForwardedToRef<aExp>() 
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    let CharParse, cref = createParserForwardedToRef<cExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse] //and here TermParse is updated
    
    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse] //and here ProdParse is updated
    
    let NParse   = pint32 |>> N <?> "Int"
    let PVParse = pstring "pointValue" >*>. pstring "(" >*>. TermParse .>*> pstring ")" |>> PV <?> "PV"
    let ParParse = parenthesise TermParse
    let NegParse = pchar '-' >>. pint32 |>> fun x -> Mul (N -1, N x) 
    let VParse = many1 (palphanumeric <|> pchar '_') |>> charListToStr |>> V
    let CharIntParse = pstring "charToInt" >*>. pstring "(" >*>. CharParse .>*> pstring ")" |>> CharToInt
    do aref := choice [NegParse; NParse; CharIntParse; ParParse; PVParse; VParse] //and here AtomParse is updated
    
    //this is a workaround for sure, but NegParse is higher in choice than NParse so that Neg is parsed as a negative
    // but on Teams this seems to be correct

    //below are the functions for CexpParse
    
    let CParse = pchar '\'' >>. (palphanumeric <|> whitespaceChar) .>> pchar '\'' |>> C
    let UpParse = pstring "toUpper" >*>. pstring "(" >*>. CharParse .>*> pstring ")"  |>> ToUpper
    let DownParse = pstring "toLower" >*>. pstring "(" >*>. CharParse .>*> pstring ")" |>> ToLower
    let IntCharParse = pstring "intToChar" >*>. pstring "(" >*>. TermParse .>*> pstring ")" |>> IntToChar
    let CharValParse = pstring "charValue" >*>. pstring "(" >*>. TermParse .>*> pstring ")" |>> CV
    //let PareParse = parenthesise CharParse
    do cref := choice [CParse; UpParse; DownParse; IntCharParse; CharValParse]
    
    let rec AexpParse = TermParse 
    and CexpParse = CharParse
        
    //Bexp
    
    let BoolParse, bref = createParserForwardedToRef<bExp>()
    let MidParse, mref = createParserForwardedToRef<bExp>()
    let EndParse, eref = createParserForwardedToRef<bExp>()
    
    let UnionParse = binop (pstring "\/") (MidParse |>> Not) (BoolParse |>> Not) |>> Conj |>> Not <?> "Union"
    let ConjParse = binop (pstring "/\\") MidParse BoolParse |>> Conj <?> "Conj"
    do bref := choice [UnionParse; ConjParse; MidParse] 
    
    let EqParse = binop (pstring "=") TermParse TermParse |>> AEq <?> "AEq"
    let NEqParse = binop (pstring "<>") TermParse TermParse |>> AEq |>> Not <?> "NEq"
    let ALtParse = binop (pstring "<") TermParse TermParse |>> ALt <?> "ALt"
    let ALtEqParse = binop (pstring "<=") TermParse TermParse |>> 
                     (fun (term1, term2) ->
                            Conj ((Not (ALt (term1, term2))), (Not (Not (Not (AEq (term1, term2))))))) |>> Not <?> "ALtEq"
    let AMtParse = binop (pstring ">") TermParse TermParse |>>
                   (fun (term1, term2) -> Conj (Not (AEq (term1, term2)),Not (ALt (term1, term2)))) <?> "AMt"
                   
    let AMtEqParse = binop (pstring ">=") TermParse TermParse |>> ALt |>> Not <?> "AMtEq"
    do mref := choice [EqParse; NEqParse; ALtParse; ALtEqParse; AMtParse; AMtEqParse; EndParse] 
    
    let ParBoolParse = parenthesise BoolParse <?> "BoolParenthesis"
    let TrueParse = pstring "true" |>> (fun _ -> TT) <?> "TT"
    let FalseParse = pstring "false" |>> (fun _ -> FF) <?> "FF"
    
    let NegatParse = pstring "~" >>. BoolParse |>> Not <?> "Negat"
    
    let IsVowelParse = pstring "isVowel" >>. CharParse |>> IsVowel <?> "IsVowel"
    let IsDigitParse = pstring "isDigit" >>. CharParse |>> IsDigit <?> "IsDigit"
    let IsLetterParse = pstring "isLetter" >>. CharParse |>> IsLetter <?> "IsLetter"
    
    do eref := choice [ParBoolParse; TrueParse; FalseParse; NegatParse; IsVowelParse; IsDigitParse; IsLetterParse]
    
    let BexpParse = BoolParse

    
    //Stmt Parse
    
    let StmtParse, sref = createParserForwardedToRef<stm>()
    let MiddleParse, midref = createParserForwardedToRef<stm>()
    let LittleParse, lref = createParserForwardedToRef<stm>()
    
    let SeqParse = MiddleParse .>*> pstring ";" .>*>. StmtParse |>> Seq <?> "Seq"
    do sref := choice [SeqParse; MiddleParse]

    let ITEParse = pstring "if" >*>. BoolParse .>*> pstring "then"
                   .>*>. MiddleParse .>*> pstring "else"
                   .>*>. MiddleParse |>>
                        (fun ((term1, term2), term3) -> ITE (term1, term2, term3)) <?> "ITE"
    
    let ITParse = pstring "if" >*>. BoolParse .>*> pstring "then"
                   .>*>. MiddleParse |>>
                        (fun (term1, term2) -> ITE (term1, term2, Skip)) <?> "IT"
                        
    let WhileParse = pstring "while" >*>. BoolParse .>*> pstring "do" .>*>. MiddleParse
                    |>> While <?> "While"
    do midref := choice [ITEParse; ITParse; WhileParse; LittleParse]
                       
    
    let BaseParse = (many1 (palphanumeric <|> pchar '_') |>> charListToStr) .>*> pstring ":=" .>*>. TermParse |>>
                    (fun (term1, term2) -> Ass (term1, term2)) <?> "Ass"
                    
    
    //can't I just add a space after declare? I sure can
    let DeclareParse = pstring "declare " >*>. many palphanumeric |>> charListToStr |>> Declare <?> "Declare"
    let ParentParse = curlisise StmtParse <?> "StmntCurlyBrackets"
    do lref := choice [BaseParse; DeclareParse; ParentParse]
    
    let stmntParse = StmtParse

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
    
    let parseSquareProg (sqp:squareProg) =
        Map.map (fun _ i -> (getSuccess (run stmntParse i)) |> stmntToSquareFun) sqp 
    
    let parseBoardProg (s:string) (sqp:Map<int, square>) = 
        getSuccess (run StmtParse s) |> (fun s -> stmntToBoardFun s sqp)
    
    let mkBoard (bp : boardProg) : board =
        let m' = Map.map (fun _ i -> parseSquareProg i) bp.squares
        {
            center = bp.center
            defaultSquare = Map.find bp.usedSquare m'
            squares = parseBoardProg bp.prog m'
            
        }
    
    // Default (unusable) board in case you are not implementing a parser for the DSL.
    // let mkBoard : boardProg -> board = fun _ -> {center = (0,0); defaultSquare = Map.empty; squares = fun _ -> Success (Some Map.empty)}

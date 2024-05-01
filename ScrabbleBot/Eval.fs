// Insert your updated Eval.fs file here from Assignment 7. All modules must be internal.

module internal Eval

    open StateMonad
    open System

    let add a b =
        a >>= fun x ->
        b >>= fun y ->
        ret (x + y)          
    let div a b =
        a >>= fun x ->
        b >>= fun y ->
        if y = 0 then
            fail DivisionByZero
        else
            ret (x / y)     

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
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsConsonant of cExp  (* check for constant *)

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

    let rec arithEval (exp : aExp) : SM<int> =
        match exp with
        | N n -> ret n
        | V var -> lookup var
        | WL -> wordLength
        | PV e ->
            arithEval e >>= fun pos ->
            pointValue pos
        | Add (a, b) ->
            arithEval a >>= fun x ->
            arithEval b >>= fun y ->
            ret (x + y)
        | Sub (a, b) ->
            arithEval a >>= fun x ->
            arithEval b >>= fun y ->
            ret (x - y)
        | Mul (a, b) ->
            arithEval a >>= fun x ->
            arithEval b >>= fun y ->
            ret (x * y)
        | Div (a, b) ->
            arithEval a >>= fun x ->
            arithEval b >>= fun y ->
            if y = 0 then
                fail DivisionByZero
            else
                ret (x / y)
        | Mod (a, b) ->
            arithEval a >>= fun x ->
            arithEval b >>= fun y ->
            if y = 0 then
                fail DivisionByZero
            else
                ret (x % y)
        | CharToInt cExp -> 
            charEval cExp >>= fun char ->
            ret (int char)

    and charEval (exp : cExp) : SM<char> =
        match exp with
        | C c -> ret c
        | CV aExp ->
            arithEval aExp >>= fun pos ->
            characterValue pos
        | ToUpper cExp ->
            charEval cExp >>= fun char ->
            ret (Char.ToUpper char)
        | ToLower cExp ->
            charEval cExp >>= fun char ->
            ret (Char.ToLower char)
        | IntToChar aExp ->
            arithEval aExp >>= fun intValue ->
            ret (char intValue)

    and boolEval (exp : bExp) : SM<bool> =
        match exp with
        | TT -> ret true
        | FF -> ret false
        | AEq (a, b) ->
            arithEval a >>= fun x ->
            arithEval b >>= fun y ->
            ret (x = y)
        | ALt (a, b) ->
            arithEval a >>= fun x ->
            arithEval b >>= fun y ->
            ret (x < y)
        | Not bExp ->
            boolEval bExp >>= fun b ->
            ret (not b)
        | Conj (b1, b2) ->
            boolEval b1 >>= fun x ->
            boolEval b2 >>= fun y ->
            ret (x && y)
        | IsVowel cExp ->
            charEval cExp >>= fun c ->
            ret (c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u' ||
                c = 'A' || c = 'E' || c = 'I' || c = 'O' || c = 'U')
        // | IsLetter cExp ->
        //     charEval cExp >>= fun c ->
        //     ret (Char.IsLetter c)
        // | IsDigit cExp ->
        //     charEval cExp >>= fun c ->
        //     ret (Char.IsDigit c)


    type stm =                (* statements *)
    | Declare of string       (* variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* nop *)
    | Seq of stm * stm        (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm     (* while statement *)

    // let rec stmntEval stmnt : SM<unit> = failwith "Not implemented"
    let rec stmntEval stmnt : SM<unit> =
        match stmnt with
        | Declare str -> declare str
        | Ass (varName, expr) -> 
            arithEval expr >>= fun value -> 
            update varName value
        | Skip -> pop
        | Seq (stmnt1, stmnt2) -> 
            stmntEval stmnt1 >>>= stmntEval stmnt2
        | ITE (cond, stmnt1, stmnt2) -> 
            push >>>= boolEval cond >>= fun result -> 
            if result then stmntEval stmnt1 >>>= pop 
            else stmntEval stmnt2 >>>= pop
        | While (cond, loopStmnt) -> 
            push >>>= stmntEval (While (cond, loopStmnt)) >>>= pop
        



   (* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    // let arithEval2 a = failwith "Not implemented"
    // let charEval2 c = failwith "Not implemented"
    // let rec boolEval2 b = failwith "Not implemented"
    //
    // let stmntEval2 stm = failwith "Not implemented"

    (* Part 4 *) 

    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>

    // let stmntToSquareFun stm = failwith "Not implemented"


    type coord = int * int

    type boardFun = coord -> Result<squareFun option, Error> 

    let stmntToBoardFun stm m = failwith "Not implemented"

    type board = {
        center        : coord
        defaultSquare : squareFun
        squares       : boardFun
    }

    let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"
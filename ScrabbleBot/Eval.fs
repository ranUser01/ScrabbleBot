module internal Eval

    open StateMonad

    let add a b =
        a >>= fun x -> 
        b >>= fun y ->
            ret (x+y) //monad applies this function to the unpacked SM and then packs it up again
            
    let sub a b =
        a >>= fun x ->
        b >>= fun y ->
            ret (x-y)
            
    let mul a b =
        a >>= fun x ->
        b >>= fun y ->
            ret (x*y)
            
    let div a b =
        a >>= fun x -> 
        b >>= fun y ->
            if y <> 0 then ret (x / y) else fail DivisionByZero
            
    let modulo a b =
        a >>= fun x ->
        b >>= fun y ->
            if y <> 0 then ret (x % y) else fail DivisionByZero    

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
       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)

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
         | N n -> ret n
         | V v -> lookup v
         | WL -> wordLength
         | PV e ->
            arithEval e >>= pointValue
         | Add (a1, a2) -> add (arithEval a1) (arithEval a2)
         | Sub (a1, a2) -> sub (arithEval a1) (arithEval a2)
         | Mul (a1, a2) -> mul (arithEval a1) (arithEval a2)
         | Div (a1, a2) -> div (arithEval a1) (arithEval a2)
         | Mod (a1, a2) -> modulo (arithEval a1) (arithEval a2) 
         | CharToInt c ->                        
              charEval c >>= fun c -> arithEval (N (int c))

    and charEval c : SM<char> =
       match c with 
           | C c -> ret c
           | CV a ->  (* Character lookup at word index *)
               (arithEval a) >>= characterValue 
           | ToUpper c ->
               (charEval c) >>= fun c -> charEval (C (System.Char.ToUpper c))
           | ToLower c ->
               (charEval c) >>= fun c -> charEval (C (System.Char.ToLower c))
           | IntToChar a -> 
                (arithEval a) >>= fun c -> charEval (C (char c)) //converts a unicode code point to char, 65 -> A

    let rec boolEval b : SM<bool> =
       match b with
       | TT -> ret true                   (* true *)
       | FF -> ret false                 (* false *)

       | AEq (a, b) ->
           arithEval a >>= fun x ->
           arithEval b >>= fun y ->
               ret (x=y)  (* numeric equality *)
       | ALt (a, b) -> 
            arithEval a >>= fun x -> (* numeric less than *)
            arithEval b >>= fun y ->
                ret (x < y)

       | Not b ->
           boolEval b >>= fun x -> ret (not x)  (* boolean not *)
       | Conj (a, b) ->
           boolEval a >>= fun x ->
           boolEval b >>= fun y ->
               ret (x && y) (* boolean conjunction *)

       | IsVowel c ->
           let vowels = ['A';'E';'I';'O';'U']
           charEval (ToUpper c) >>= fun x ->
               ret (List.contains x vowels) (* check for vowel *)
       | IsConsonant c ->
           let vowels = ['A';'E';'I';'O';'U']
           charEval (ToUpper c) >>= fun x ->
               ret (System.Char.IsLetter x && (List.contains x vowels)) (* check for consonant *)
       | IsLetter c ->
           charEval c >>= fun x -> ret (System.Char.IsLetter x) (* check for letter *)
       | IsDigit c ->
           charEval c >>= fun x -> ret (System.Char.IsDigit x) (* check for digit *)


    type stm =                (* statements *)
    | Declare of string       (* variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* nop *)
    | Seq of stm * stm        (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm     (* while statement *)

    let rec stmntEval stmnt : SM<unit> =
        match stmnt with
        | Declare s -> declare s
        | Skip -> ret ()
        | Ass (s,a) -> arithEval a >>= update s
        | Seq (stm1, stm2) -> stmntEval stm1 >>>= stmntEval stm2
        | ITE (b, stm1, stm2) ->
                push >>>= boolEval b >>=
                (fun x ->
                    if (x = true)
                    then stmntEval stm1
                    else stmntEval stm2) >>>= pop
                
        | While (b, stm) ->
            push >>>= boolEval b >>=
                (fun x ->
                    if (x = true)
                    then stmntEval stm >>>= stmntEval (While (b, stm)) 
                    else ret ()) //does nothing, but idk how to do it in another way
                    >>>= pop 

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder() //this creates an environment with members

    let binop2 f a b =
        prog {
            let! x = a
            let! y = b
            return f x y
        }
    
    let divop f a b = prog {
        let! x = a
        let! y = b
        if y <> 0 then
            return f x y
        else
            return! fail DivisionByZero }

    let rec arithEval2 a = prog {
         match a with
         | N n -> return n
         | V v -> return! lookup v 
         | WL -> return! wordLength
         | PV e ->
            let! aux = arithEval2 e
            return! pointValue aux
         | Add (a1, a2) -> return! binop2 ( + ) (arithEval2 a1) (arithEval2 a2)
         | Sub (a1, a2) -> return! binop2 ( - ) (arithEval2 a1) (arithEval2 a2)
         | Mul (a1, a2) -> return! binop2 ( * ) (arithEval2 a1) (arithEval2 a2)
         | Div (a1, a2) -> return! divop ( / ) (arithEval2 a1) (arithEval2 a2)
         | Mod (a1, a2) -> return! divop ( % ) (arithEval2 a1) (arithEval2 a2) 
         | CharToInt c ->
            let! aux = charEval2 c
            return! arithEval2 (N (int aux))}


    and charEval2 c = prog {
        match c with 
           | C c -> return c
           | CV a ->  (* Character lookup at word index *)
               let! aux = arithEval2 a
               return! characterValue aux
           | ToUpper c ->
               let! aux = charEval2 c
               return! charEval2 (C (System.Char.ToUpper aux))
           | ToLower c ->
               let! aux = charEval2 c
               return! charEval2 (C (System.Char.ToLower aux))
           | IntToChar a ->
                let! aux  = arithEval2 a
                return! charEval2 (C (char aux)) //converts a unicode code point to char, 65 -> A
                }
    
    let rec boolEval2 b = prog {
        match b with
           | TT -> return true                   (* true *)
           | FF -> return false                 (* false *)

           | AEq (a, b) ->
               let! aux1 = arithEval2 a
               let! aux2 = arithEval2 b
               return (aux1=aux2)
               
           | ALt (a, b) ->
               let! aux1 = arithEval2 a
               let! aux2 = arithEval2 b
               return (aux1 < aux2)

           | Not b ->
               let! aux = boolEval2 b
               return (not aux)  (* boolean not *)
               
           | Conj (a, b) ->
               let! aux1 = boolEval2 a
               let! aux2 = boolEval2 b
               return (aux1 && aux2) (* boolean conjunction *)

           | IsVowel c ->
               let vowels = ['A';'E';'I';'O';'U';'Y']
               let! aux = charEval2 (ToUpper c)
               return (List.contains aux vowels) (* check for vowel *)
               
           | IsLetter c ->
               let! aux = charEval2 (ToUpper c)
               return (System.Char.IsLetter aux) (* check for letter *)
           | IsDigit c ->
               let! aux = charEval2 (ToUpper c)
               return (System.Char.IsDigit aux) (* check for digit *)
               }
               
    let rec stmntEval2 stm = prog {
        match stm with
        | Declare s -> return! declare s
        | Skip -> return ()
        | Ass (s,a) ->
            let! aux = arithEval2 a
            return! update s aux
        | Seq (stm1, stm2) ->
            do! stmntEval2 stm1 
            return! stmntEval2 stm2
            
        | ITE (b, stm1, stm2) ->
                let! aux = boolEval2 b
                if (aux = true)
                then 
                    do! push
                    do! stmntEval stm1 
                    do! pop //return here, it's unit() anyway          
                else 
                    do! push
                    do! stmntEval stm2
                    do! pop
        | While (b, stm) ->
            let! aux = boolEval2 b
            if (aux = true)
            then
                do! push
                do! stmntEval stm
                do! stmntEval (While (b, stm))
                do! pop //can I just do that without returning?
            else
                return ()
    }

(* Part 4 *) 

    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    
    let stmntToSquareFun stm : squareFun = fun (w:word) (pos:int) (acc:int) ->
       let s = mkState [("_pos_", pos); ("_acc_", acc); ("_result_", 0)] w ["_pos_"; "_acc_"; "_result_"]
       stmntEval2 stm >>>= lookup "_result_" |> evalSM s
    
    type square = Map<int, squareFun> //priorities not used for now
    type coord = int * int

    type boardFun = coord -> Result<square option, Error> //square with the updated evaluator
      
    let rec stmntToBoardFun stm (squares: Map<int, square>) = fun (cord:coord) ->
        let s = mkState [("_x_", fst cord); ("_y_", snd cord); ("_result_", 0)] [] ["_x_"; "_y_"; "_result_"]

        prog {
            do! stmntEval stm 
            let! id = lookup "_result_"
            return Map.tryFind id squares }
        |> evalSM s
              
       
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun
    }

    let mkBoard c defaultSq boardStmnt ids =
        {center = c
         defaultSquare = Map.find defaultSq ids
         squares = stmntToBoardFun boardStmnt ids }
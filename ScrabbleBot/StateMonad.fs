// Insert your StateMonad.fs from Assignment 6 here. All modules must be internal.


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

    let pop : SM<unit> = S (fun s ->
        match s.vars with 
        | _ :: xs -> Success ((), {s with vars = xs}) 
        | [] -> Failure (VarNotFound "Cannot pop from an empty stack")
        ) ;;

    let wordLength : SM<int> = S (fun s ->  Success (s.word.Length, s))   ;;

    let characterValue (pos : int) : SM<char> = S (fun s ->
        if s.word.Length >= 0 && pos <= s.word.Length - 1 then
            match s.word[pos] with 
                | character, _ -> Success (character, s)
        else
            Failure (IndexOutOfBounds pos)
            ) ;;  

    let pointValue (pos : int) : SM<int> = S (fun s ->
        if pos >= 0 && pos <= s.word.Length - 1 then
            match s.word[pos] with 
                | _, value -> Success (value, s)
        else
            Failure (IndexOutOfBounds pos)
            ) ;;     

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
                | None   -> Failure (VarNotFound x)) ;;


    let update (var : string) (value : int) : SM<unit> =
        let rec aux index =
            function
            | []      -> None
            | m :: ms -> 
                match Map.tryFind var m with
                | Some _ -> Some (index, m.Add (var, value))
                | None   -> aux (index + 1) ms 

        S (fun s -> 
                match aux 0 (s.vars) with
                | Some (index, updatedMap) -> 
                let updatedMaps = List.mapi (fun i m -> if i = index then updatedMap else m) s.vars
                Success ((), { s with vars = updatedMaps })
                | None   -> Failure (VarNotFound var)) ;;

    let declare (var : string) =
        S (fun s ->
            if not (s.reserved.Contains(var)) then
                match s.vars with
                    | [] -> Failure (VarNotFound "Stack is empty")
                    | x :: xs -> 
                        match Map.tryFind var x with
                            | Some _ -> Failure (VarExists var)
                            | None   -> Success ((), {s with vars = x.Add (var,0)::xs}) 
            else 
                Failure (ReservedName var)
        )
    ;;
module internal Dictionary

type Dict = Node of bool * Map<char,Dict>
    
let empty = fun () -> Node (false, Map.empty)
    
let rec insert (s:string) (dict:Dict) =
    
    //HELPERS
    let chars = s |> Seq.toList //convert string to a list of chars
    let char = if chars.Length >= 1 then chars.Head else ' ' //get the first char
    let new_s = if chars.Length >= 1 then System.String.Concat(Array.ofList(chars.Tail)) else "" //return remainder
       
    match dict with
    //Given a node (like the root), add a child to it (Map entry) with the char at hand
    | Node (b, m) when chars.Length >= 1 ->
        let next = Map.tryFind char m |> Option.defaultValue (empty ())
        Node (b, Map.add char (insert new_s next) m)
    //When you run out of chars to add, flag the current node as true and leave the map be
    | Node (_, m) when chars.Length = 0 -> Node (true, m)
let rec lookup (s:string) (dict:Dict) =
    
    //HELPERS
    let chars = s |> Seq.toList //convert string to a list of chars
    let char = if chars.Length >= 1 then chars.Head else ' ' //get the first char
    let new_s = if chars.Length >= 1 then System.String.Concat(Array.ofList(chars.Tail)) else "" //return remainder
    
    match dict with
    | Node (_, m) when chars.Length >= 1 ->
        let next = Map.tryFind char m |> Option.defaultValue (empty ())
        if next <> empty () then (lookup new_s next) else false
    //When you run out of chars to add, flag the current node as true and leave the map be
    | Node (true, _) when chars.Length = 0 -> true
    | Node (false, _) when chars.Length = 0 -> false
    
let step (c:char) (dict:Dict) =
    match dict with
    | Node (_,m) ->
        let next = Map.tryFind c m |> Option.defaultValue (empty ())
        if next <> empty () then
            match next with
            | Node (false, _) -> Some (false, next)
            | Node (true, _) -> Some (true, next)
        else None
            
    
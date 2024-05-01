// Insert your MultiSet.fs file here. All modules must be internal

module internal MultiSet

    type MultiSet<'a when 'a: comparison> = R of Map<'a, uint32>

    let empty = R Map.empty
    
    let numItems (a: 'a) (R s) : uint32 =
        match Map.tryFind a s with
        | Some count -> count
        | None -> 0u
        
    let add (a: 'a) (n: uint32) (R s) : MultiSet<'a> =
        if n = 0u then
            R s
        else
            let currentNumItems = numItems a (R s)
            R (Map.add a (currentNumItems + n) s)
    let fold (f: 'b -> 'a -> uint32 -> 'b) (acc: 'b) (R s) : 'b =
        Map.fold (fun acc k v -> f acc k v) acc s

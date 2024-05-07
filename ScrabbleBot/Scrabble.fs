namespace Bot 

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.

(* 
Multi-player -> no
Dictionary -> yes
playing on all boards -> no
parallelism -> no
Respect the timeout flag -> no
*)

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player number, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        board         : Parser.board
        dict          : Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
        usedTiles     : Map<coord, char> //a mapping from coord to char, for iterating using fold
        playingNow    : uint32
        numberOfPlayers: uint32
        tilesInGame   : int

    }

    let mkState b d pn h ut pno nop tig = {board = b; dict = d;  playerNumber = pn; hand = h; usedTiles = ut; playingNow = pno; numberOfPlayers = nop; tilesInGame = tig }

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    let usedTiles st     = st.usedTiles

                       
module Scrabble =
    open System.Threading

    let playGame cstream pieces (st : State.state) =
        
        // FINDING WORDS IN THE DICTIONARY
        //=======================================================================================
        let findChar i:char = //looks up a char by id in pieces
            Map.find i pieces |> Set.toList |>
                function
                | first::rest -> fst first //always takes the first Letter, also for jokers
                | [] -> 'A' //should never happen
        let findVal i:int = //looks up a char by id in pieces
            Map.find i pieces |> Set.toList |>
                function
                | first::rest -> snd first //always takes the first Letter, also for jokers
                | [] -> 0 //should never happen
                
        // Finds all possible words given a dictionary (or a sub-dictionary while traversing down the trie) and the hand
        // char list * uint32 list keeps track of chars and corresponding ids for placement
        let rec traverseTrie (dict: Dictionary.Dict) (hand: MultiSet.MultiSet<uint32>) (word: (char * uint32) list) (allWords: ((char * uint32) list) list) =
            // smart idea for a data structure creation, pass an empty accumulator at the beginning and return it filled at the end of recursion
            MultiSet.fold (fun (wordAcc, allWordsAcc) key value -> 
                match Dictionary.step (findChar key) dict with
                | None -> (wordAcc, allWordsAcc)
                | Some (false, nextDict) ->
                   // we return wordAcc (empty) as the first argument, as eventually all that interests us is the second accumulator
                   let wordAccNext = wordAcc@[findChar key, key] 
                   (wordAcc, snd (traverseTrie nextDict (MultiSet.removeSingle key hand) wordAccNext allWordsAcc))
                | Some (true, nextDict) ->
                    let wordAccNext = wordAcc@[findChar key, key] 
                    let allWordsAccNext = (wordAccNext::allWordsAcc)
                    (wordAcc, snd (traverseTrie nextDict (MultiSet.removeSingle key hand) wordAccNext allWordsAccNext))
                       
                ) (word, allWords) hand
          
        // return the first word from the list of found words - or any other heuristic
        let returnBest (foundWords: ((char * uint32) list) list) =
            match foundWords with
            | first::_ -> first
            | [] -> [] //returns an empty list if no word is found
            
        // take the state and return the best word with char ids
        let findWord (st:State.state) = returnBest (snd (traverseTrie st.dict st.hand [] [[]]))
        
        //returns the dictionary after the word already on the board has been traversed
        //possible to extend to full words, for now it's a singleton char list to start at a letter
        let traverseForWord (dict:Dictionary.Dict) (word: char list) =
            List.fold (fun dictAcc character -> // I don't need a lookup here, as it's a word found on the board
                match Dictionary.step character dictAcc with
                | None -> dict
                | Some (_, newDict) -> newDict //false or true doesn't matter, it's a word already on the board
                ) dict word
         
        //find word with a beginning as another word
        let findWordWithStart (st:State.state) (word:char list) =
            //I don't need the starting word itself in memory, hence empty lists
            returnBest (snd (traverseTrie (traverseForWord st.dict word) st.hand [] [[]]))
        
        // NAVIGATING THE BOARD
        //=======================================================================================
        
        let directions = ["right"; "left"; "up"; "down"]
        
        //take a coordinate, the map of the board and the direction, return false or true depending if there is a tile there
        let neighborValue (usedTiles: Map<coord, char>) (cord:coord) (direction:string) =
            match direction with
            |"right" -> (fst cord) + 1 , snd cord
            |"left"  -> (fst cord) - 1 , snd cord
            |"up"    -> (fst cord) , (snd cord) - 1
            |"down"  -> (fst cord) , (snd cord) + 1
            |> fun direction -> match usedTiles.TryFind direction with
                                | None -> false
                                | Some value -> true
        
        //given a coordinate and the board, return a map of directions with booleans for the presence or absence of a neighbor
        let neighborMap (usedTiles: Map<coord, char>) (cord: coord) =
            List.fold (fun mapAcc value ->
                Map.add value (neighborValue usedTiles cord value) mapAcc) Map.empty directions
            
        //given coordinates, take a look at each neighbor, then give the possible direction of writing the word
        let viableDirection (usedTiles: Map<coord, char>) (cord: coord) =
            let neighbors = neighborMap usedTiles cord
            match Map.find "right" neighbors with
            | true -> match Map.find "up" neighbors with
                      | true -> None //if neighbors up and right, don't write
                      | false -> match Map.find "down" neighbors with
                                 | true -> None
                                 | false -> Some "down" //if no neighbors up and down, write down
            | false -> match Map.find "left" neighbors with
                       | true -> None
                       | false -> Some "right" //if no neighbors right and left, write right
              
        // return each letter we can start writing the word from and its cords given the board state         
        let startingLetters (usedTiles: Map<coord, char>) =
            Map.fold (fun letterListAcc cord character ->
                match viableDirection usedTiles cord with
                | None -> letterListAcc
                | Some direction -> ((character, cord), direction)::letterListAcc
                ) List.empty usedTiles
            
        // PLACING THE FOUND WORD
        //=======================================================================================
        
        // given a direction and a coordinate, return one step further in that direction. Helper for the next function
        let makeIncrement (i:int) (direction:string) (cord:coord) = 
            match direction with
            | "right" -> (fst cord) + i, snd cord
            | "down"  -> fst cord, (snd cord) + i //i is how much of an increment you want to do
        
        //given a letter at which to start, find best word using tiles at your hand and return it with coordinates
        let placeWordFromLetter (letter: (char*coord)*string) (st: State.state) =
            let letterAsCharList = fst letter |> fst |> List.singleton
            let direction = snd letter
            let cord = fst letter |> snd 
            let subsequentLetters = findWordWithStart st letterAsCharList //char list * uint32 list
              
            List.fold (fun (listAcc, increment) element -> 
                listAcc@[element, makeIncrement (increment+1) direction cord], increment+1) (List.empty, 0) subsequentLetters
            
        // after finding a word to place, check if placing it won't touch other words. Return true if not, false otherwise
        let validityCheck (placedWord:((char*uint32) * (int*int)) list) (usedTiles: Map<coord,char>) =
            fst (List.fold (fun (boolVal, count) element ->
                match count with
                // >1 because the first letter placed will have one neighbor, the starting letter already on the board
                | 0 -> if (Map.fold (fun numberOfTrueNeighbors direction isTileBool ->
                            if isTileBool then numberOfTrueNeighbors+1 else numberOfTrueNeighbors)
                                0 (neighborMap usedTiles (snd element))) > 1 //snd element is coord
                       then (false, 1) //run this one, change count to 1
                       else (boolVal, 1)
                // >0 because the next letters placed should not touch any other tiles
                | _ -> if (Map.fold (fun numberOfTrueNeighbors direction isTileBool ->
                            if isTileBool then numberOfTrueNeighbors+1 else numberOfTrueNeighbors)
                                0 (neighborMap usedTiles (snd element))) > 0 //snd element is coord
                       then (false, count)
                       else (boolVal, count)) (true, 0) placedWord)
            
        // make a list of possible words to play, one for each letter after the validity check
        let getWordsForEachLetter (st:State.state) (letterList: ((char*coord)*string) list) =
            List.fold (fun wordListAcc letter ->
                            if validityCheck (fst (placeWordFromLetter letter st)) st.usedTiles //discard all best word that don't pass the test
                            then wordListAcc@[fst (placeWordFromLetter letter st)] //add words that pass the test
                            else wordListAcc) List.empty letterList
        
        // any heuristic - take the first word from the whole list to play    
        let getBestWord (st:State.state) =
            let aux (wordList:((char * uint32) * (int*int)) list list) =
                match wordList with
                | first::_ -> first
                | [] -> [] //if no move, this is returned
            
            startingLetters st.usedTiles |> getWordsForEachLetter st |> aux //calls the actual logic
        
        // find the best word using hand only and add coordinates starting at the center     
        let getBestWordFromHand (st:State.state) =
            let word = findWord st
            List.fold (fun (listAcc, increment) character ->
                            listAcc@[(character, makeIncrement (increment+1) "right" st.board.center)], increment+1) (List.empty, -word.Length) word  //- to end at position 0 to get more space to the right
        
        // modifies the found word to the format used to send to the server
        let wordToCommand (word:((char*uint32)*coord) list) =
            List.fold (fun finalCommand character ->
                           finalCommand@[(snd character, ((fst character |> snd), ((fst character |> fst), findVal (fst character |> snd))))])
                           List.empty word
         
        // compiles the final move depending on the board being empty or not                   
        let Move (st:State.state) = if st.usedTiles.IsEmpty then wordToCommand (fst (getBestWordFromHand st)) else wordToCommand (getBestWord st)
        //==============================================================================================
        
        let rec aux (st : State.state) =
            //Print.printHand pieces (State.hand st)

            //remove the force print when you move on from manual input (or when you have learnt the format)
            //forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            //let input = System.Console.ReadLine()     
            //let move = RegEx.parseMove input
            
            // exchanging pieces logic
            let piecesInBag = st.tilesInGame - (Map.count st.usedTiles) - (7*int(st.numberOfPlayers)) //pieces on board and max in hands
            let exchange = fst (MultiSet.fold (fun (idsAcc, count) tileID _ -> //doesn't take duplicates into account, but that's fine. Worst case n less tiles is exchanged
                                            if count <= piecesInBag then idsAcc@[tileID], count+1 else idsAcc, count) (List.empty, 1) st.hand) //hard-coded 3 to make the bot not exchange too much
            
            if (st.playingNow = st.playerNumber) then
                let move = Move st

                // play, pass or exchange
                let my_move =
                    match move with
                    | [] ->
                        match exchange with
                        | [] -> SMPass
                        | _ -> SMChange exchange
                    | _ -> SMPlay move
                
                //debugPrint(sprintf "Me : %A" st.playerNumber)
                //debugPrint(sprintf "currentPlayer: %A" st.playingNow)         
                //debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                send cstream my_move

            let msg = recv cstream
            //debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            
            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) -> //ms = move, points - ignore, newPieces - add t hand
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let addToBoard = List.fold (fun mapAcc element ->
                                Map.add (fst element) (snd element |> snd |> fst) mapAcc) st.usedTiles ms
                //the below produces newHand
                let removeFromHand = List.fold (fun multiAcc element ->
                                MultiSet.removeSingle (snd element |> fst) multiAcc) st.hand ms
                //I presume newPieces are of same length as ms
                let addToHand newHand = List.fold (fun multiAcc element ->
                                MultiSet.add (fst element) (snd element) multiAcc) newHand newPieces
                // hard-code changing bots from 1 to 2 and 2 to 1
                let changePlayer = if (st.playerNumber = 1u) then 2u else 1u
                let st' = State.mkState st.board st.dict st.playerNumber (removeFromHand |> addToHand) addToBoard changePlayer st.numberOfPlayers st.tilesInGame
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let addToBoard = List.fold (fun mapAcc element ->
                                Map.add (fst element) (snd element |> snd |> fst) mapAcc) st.usedTiles ms
                let changePlayer = if (pid = 1u) then 2u else 1u
                let st' = State.mkState st.board st.dict st.playerNumber st.hand addToBoard changePlayer st.numberOfPlayers st.tilesInGame
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *) //no change here from me, as I don't care about turn navigation now
                let changePlayer = if (pid = 1u) then 2u else 1u
                let st' = State.mkState st.board st.dict st.playerNumber st.hand st.usedTiles changePlayer st.numberOfPlayers st.tilesInGame
                aux st'
            | RCM (CMChangeSuccess newPieces) ->
                let removeFromHand = List.fold (fun multiAcc element ->
                                MultiSet.removeSingle element multiAcc) st.hand exchange
                //I presume newPieces are of same length as exchange
                let addToHand newHand = List.fold (fun multiAcc element ->
                                MultiSet.add (fst element) (snd element) multiAcc) newHand newPieces
                let changePlayer = if (st.playerNumber = 1u) then 2u else 1u
                let st' = State.mkState st.board st.dict st.playerNumber (removeFromHand |> addToHand) st.usedTiles changePlayer st.numberOfPlayers (st.tilesInGame - newPieces.Length)
                aux st'    
            | RCM (CMChange (pid, numberOfTiles)) ->
                let changePlayer = if (pid = 1u) then 2u else 1u
                let st' = State.mkState st.board st.dict st.playerNumber st.hand st.usedTiles changePlayer st.numberOfPlayers (st.tilesInGame - int(numberOfTiles))
                aux st'
            | RCM (CMPassed pid) ->
                let changePlayer = if (pid = 1u) then 2u else 1u
                let st' = State.mkState st.board st.dict st.playerNumber st.hand st.usedTiles changePlayer st.numberOfPlayers st.tilesInGame
                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st


        aux st

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand
        
        // official Scrabble rules say we start with a 100 pieces in the bag
        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet Map.empty playerTurn numPlayers 100) 
        
namespace Huey

open System
open System.Collections.Generic
open Microsoft.FSharp.Collections
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint
open StateMonad

// The RegEx module is only used to parse human input. It is not used for the final product.

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
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        board         : Parser.board
        dict          : ScrabbleUtil.Dictionary.Dict
        playerNumber  : uint32
        playerAmount  : uint32
        playerTurn    : uint32
        hand          : MultiSet.MultiSet<uint32>
        piecesInPlay: Map<coord, uint32> //Pair of coordinate to a tileId
        timeout : option<uint32>
        playerForfeited: Set<uint32>
    }

    let mkState b d pn h pa pt tout = {board = b; dict = d;  playerNumber = pn; hand = h; playerAmount = pa; playerTurn = pt; piecesInPlay = Map.empty; timeout = tout; playerForfeited = Set.empty;}

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand

module Scrabble =
    open System.Threading
    //Character to point value
    let charToPointValue (c: char) =
        match System.Char.ToUpper(c) with 
        | 'A' | 'E' | 'I' | 'O' | 'U' | 'L' | 'N' | 'S' | 'T' | 'R' -> 1
        | 'D' | 'G' -> 2
        | 'B' | 'C' | 'M' | 'P'  -> 3
        | 'F' | 'H' | 'V' | 'W' | 'Y' -> 4
        | 'K'-> 5
        | 'J' | 'X' -> 8
        | 'Q' | 'Z' -> 10
        | _ -> 0
        
    //Character to tile ID
    let charToTileId (c: char) = uint32(Char.ToUpper(c)) - 64u
    
    //Tile ID to Character
    let tileIdToChar (tileId: uint32) =  Convert.ToChar(tileId + 64u)
    
    //Tile ID to set of Tiles(char, pv)
    let tileIdToTile (tileId: uint32) (pieces: Map<uint32, tile>) =  Map.find tileId pieces 
    
    let stringToListOfChar (string: string) = List.ofArray (string.ToCharArray())
    
    let moveHorizontal = (1, 0)
    let moveVertical = (0, 1)
    
    //Gets the next coordinate based on direction and current position. 
    let nextCoord (currentCoord: coord) (direction: coord) = ((fst currentCoord)+(fst direction), (snd currentCoord)+(snd direction))
    
    //Gets the previous coordinate based on direction and current position. 
    let previousCoord (currentCoord: coord) (direction: coord) = ((fst currentCoord)-(fst direction), (snd currentCoord)-(snd direction))
    
    let isSquareOccupied (square: coord) (piecesInPlay: Map<coord, uint32>) =
        match Map.tryFind square piecesInPlay with
        | Some occupantId -> true
        | None -> false
    
    //Converts a word into a list of SMMove, should later be called with either moveHorizontal or moveVertical. This should be done once a valid word has been found.
    let wordToSMMove (word: List<uint32 * char>) (startCoord: coord) (direction: coord) =
         let rec aux (currentIndex: int) (coord: coord) =
             if currentIndex >= word.Length then [] else
             let char = snd word.[currentIndex]
             let tileId = fst word.[currentIndex]
             let move = (coord, (tileId, (char, charToPointValue(char))))
             let nextPosition = nextCoord coord direction
             move :: aux(currentIndex+1) (nextPosition)
         aux 0 startCoord 
    
    //concludeWord() is used in findWordsOnBoard to finalize words from the current coordinate of a starter character.
    let findWordsOnBoard (piecesInPlay: Map<coord, uint32>) =
        let rec concludeWord (wordBuilder: string) (currentCoord: coord) (direction: coord) =
            let nextPosition = nextCoord currentCoord direction
            if isSquareOccupied nextPosition piecesInPlay then //if theres a letter on the square, add it to the wordBuilder.
                let tileIdOnNextPosition = Map.find nextPosition piecesInPlay
                concludeWord (wordBuilder + string (tileIdToChar tileIdOnNextPosition)) nextPosition direction
            else wordBuilder //Once square no longer occupied, return word. 
                    
        Map.fold ( fun acc coord tileId ->
            let charAtCoord = string (tileIdToChar tileId)
            let checkLeft = isSquareOccupied(previousCoord coord moveHorizontal) piecesInPlay
            //Dummy value, since there is something on the left hand side of current character
            let horizontalWord = if checkLeft then ("!", ((0,0), moveHorizontal)) else (concludeWord charAtCoord coord moveHorizontal, (coord, moveHorizontal)) //Left is free, finish word horizontally
            
            let checkUp = isSquareOccupied(previousCoord coord moveVertical) piecesInPlay
            //Dummy value, since there is something above the current character
            let verticalWord = if checkUp then ("!", ((0,0), moveVertical)) else (concludeWord charAtCoord coord moveVertical, (coord, moveVertical)) //Up is free, finish word vertically
        
            //If the character at the current coordinate BEGINS a word, then prepend it to the list of words already on the board. 
            let tempAcc = if (fst horizontalWord) = "!" then acc else horizontalWord :: acc
            if (fst verticalWord) = "!" then tempAcc else verticalWord :: tempAcc
            ) [] piecesInPlay

        //Note to self, the word 'NODE' is a SUBTREE
        //Words that can be played from my hand, by finding all subtrees in the dictionary that correspond to the final letter of the word on the board. 
    let findPlayableWords (wordOnBoard: string) (startCoord: coord) (direction: coord) (st: State.state) (pieces: Map<uint32, tile>) =
        let subdict =
           List.fold (fun acc char -> 
               match Dictionary.step char acc with 
                 | Some (_, node) -> node
                 | None -> st.dict //st.dict is returned it has to have the same type as node, but this scenario never occurs. It will always find some sub tree because we step into a word on the board. 
           ) st.dict (stringToListOfChar wordOnBoard)
           
        let rec aux (hand: MultiSet.MultiSet<uint32>) (subdict: Dictionary.Dict) (currentWord: List<uint32 * char>) =    
            List.fold (fun acc tileId -> //For each tile in my hand. 
                let charPointvalueSet = tileIdToTile tileId pieces
                Set.fold (fun acc (char, pv) -> //For each bogstav that the tile can be. (Only wildcards can be multiple bogstaver)
                   match Dictionary.step char subdict with 
                     | Some (b, node) ->
                        let newHand = MultiSet.removeSingle tileId hand //Remove bogstav from hand if it is steppable. (we find it in the subtree)
                        if b then //b is true when the word is finished. (its a node with a true boolean)
                            (currentWord @ [(tileId, char)]) :: aux newHand node (currentWord @ [(tileId, char)]) //Prepend the finished word and recursively check for more words from new hand.
                        else 
                            aux newHand node (currentWord @ [(tileId, char)]) // recursively check for more words from new hand.
                     | None -> acc
                ) acc charPointvalueSet
            ) [] (MultiSet.toList st.hand)
        
        List.map (fun char -> ((charToTileId char), char)) (stringToListOfChar wordOnBoard) //Makes the word on board of type string into a List<uint32, char> (this is same type as currentWord)
        |> aux st.hand subdict //Calls aux to find all possible completed words from the word on the board.(this returns a list of currentWord's)
        |> List.map (fun word -> (word, (startCoord, direction)))  //Maps those possible words to a starting coord and a direction (this then maps over the list of currentWord's to a startcoordinate and direction)
        // addTwo 5  præcis det samme som   5 |> addTwo
    
    let findLongestWord (wordList: List<(List<uint32 * char>) * (coord * coord)>) =
        //Dummy value because were only interested in the actual lenght of the word.
        List.fold (fun acc ((wordList: List<uint32 * char>), (coord, direction)) ->
           if List.length wordList > List.length (fst acc) then (wordList, (coord, direction)) else acc
            ) ([], ((0, 0), moveHorizontal))  wordList
    
    //Words that can be placed on the current state of the board. 
    let findPlaceableWord (st : State.state) (pieces: Map<uint32, tile>) =
        let wordsOnBoard = findWordsOnBoard st.piecesInPlay
        let allPlaceableWords =
            List.fold (fun acc (word, (coord, direction)) -> // for each word on board ...
                let playableWords = findPlayableWords word coord direction st pieces
                let placeableWordsFromWordOnBoard =
                    List.fold (fun acc ((wordList: List<uint32 * char>), (coord, direction)) -> // for each playable word from my hand ...
                        let rec aux (currentIndex: int) (coord: coord) (pip :Map<coord, uint32>)= //pip = pieces in play ... function checks if the word can be placed?
                            if currentIndex >= word.Length then
                                let filteredBoard = List.filter (fun ((wordOnBoard:string), _ ) -> wordOnBoard.Length <> 1) (findWordsOnBoard pip)
                                List.fold (fun acc (wordOnBoard: string, _) -> // for each valid word in the temporary state ... 
                                        if Dictionary.lookup wordOnBoard st.dict then acc else false //acc is true until a word that is not in the dictionary is found, then it is set to false.
                                        ) true filteredBoard 
                            else
                                let tmpPIP = Map.add coord (fst (wordList.[currentIndex])) pip //We try to add the placeable words in a replica temporary state of PIP.
                                let nextPosition = nextCoord coord direction 
                                aux(currentIndex+1) nextPosition tmpPIP 
                        if aux 0 coord st.piecesInPlay then (wordList,(coord, direction)) :: acc //If the word CAN be placed, prepend to list and.
                            else acc //Else return current list.
                    ) [] playableWords
                placeableWordsFromWordOnBoard @ acc
            ) [] wordsOnBoard
        findLongestWord allPlaceableWords //Of all words that can be placed, return the longest one. 

    // Handle coordinates (x,y) stuff like horizontal and vertical direction of the word. Note that (0,0) is center.
    // Therefore (-1, 0) is hor left, and (0, -1) is ver up. (0,1) ver down. (1,0) hor right
    
    let nextTurn (st : State.state) = 
        { st with 
            playerTurn = (st.playerTurn % st.playerAmount) + 1u;}

    let updateHand (st : State.state) (np : list<uint32*uint32>) = 
        { st with
            hand = st.hand;} //List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand // This expects an empty hand

    let updatePiecesInPlay (st : State.state) (move : list<coord * (uint32 * (char * int))>) =
        {st with
            piecesInPlay = st.piecesInPlay;}

    let updatePlayerCount (st : State.state) (pid : uint32) = 
        { st with
            playerForfeited = st.playerForfeited.Add(pid);}

    let playGame cstream (pieces: Map<uint32, tile>) (st : State.state) =

        let rec aux (st : State.state) =
            
            match st.playerForfeited.Contains(st.playerTurn) with
            | true -> 
                let st' = nextTurn st
                aux st'
            | false ->
                match st.playerTurn = st.playerNumber with
                | true -> 
                    let placeableWord = findPlaceableWord st pieces
                    if (fst placeableWord).Length > 0 then
                        let move = wordToSMMove (fst placeableWord) (fst (snd placeableWord)) (snd (snd placeableWord)) 
                        debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                        send cstream (SMPlay move)
                    else
                        debugPrint (sprintf "Player %d -> Server:\npassed\n" (State.playerNumber st)) // keep the debug lines. They are useful.
                        send cstream (SMPass)
                | false -> 
                    debugPrint (sprintf "Waiting for my turn")
            

            //send cstream (SMForfeit) "not implemented" don not do this ever right?
            //send cstream (SMChange (uint32 list)) "not implemented" maybe do this sometimes?

            let msg = recv cstream

            match msg with
            | RCM (CMPlaySuccess(move, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                debugPrint (sprintf "Player %d <- Server:\n%A\nsuccess\n" (State.playerNumber st) move)
                let st' = updatePiecesInPlay st move
                let st'' = updateHand st' newPieces
                let st''' = nextTurn st''
                aux st'''
            | RCM (CMPlayed (pid, move, points)) ->
                (* Successful play by other player. Update your state *)
                debugPrint (sprintf "Player %d <- Server:\n%A\nsuccess\n" (pid) move)
                let st' = updatePiecesInPlay st move
                let st'' = nextTurn st'
                aux st''
            | RCM (CMPlayFailed (pid, move)) ->
                (* Failed play. Update your state *)
                debugPrint (sprintf "Player %d <- Server:\n%A\nfailed\n" (pid) move)
                let st' = nextTurn st 
                aux st'
            | RCM (CMPassed (pid)) | RCM (CMTimeout (pid)) ->
                (* Player passed. Update your state *)
                debugPrint (sprintf "Player %d <- Server:\npassed\n" (pid))
                let st' = nextTurn st
                aux st'
            | RCM (CMForfeit (pid)) ->
                (* Player left the game. Update your state *)
                debugPrint (sprintf "Player %d <- Server:\nforfeited\n" (pid))
                let st' = updatePlayerCount st pid
                let st'' = nextTurn st'
                aux st''
            | RCM (CMChange (pid, numberOfTiles)) ->
                (* Player successfully changed numberOfTiles tiles. Update your state *)
                debugPrint (sprintf "Player %d <- Server:\nchanged %d tiles\n" (pid) (numberOfTiles))
                let st' = nextTurn st 
                aux st'
            | RCM (CMChangeSuccess (tiles)) ->
                (* Successful changed tiles by you. Update your state *)
                debugPrint (sprintf "Player %d <- Server:\n\nChange success\n" (State.playerNumber st))
                let st' = updateHand st tiles
                let st'' = nextTurn st'
                aux st''
            | RCM (CMGameOver _) -> () //Loop ends i.e. Game ends.
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st

        aux st //Starts the loop

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

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet numPlayers playerTurn timeout)
        
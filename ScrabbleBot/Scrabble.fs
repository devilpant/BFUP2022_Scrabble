namespace Huey

open System
open System.Collections.Generic
open Microsoft.FSharp.Collections
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint
open StateMonad

module internal Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module internal State = 
    
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
        changeHandBool: Boolean
    }

    let mkState b d pn h pa pt tout = {board = b; dict = d;  playerNumber = pn; hand = h; playerAmount = pa; playerTurn = pt; piecesInPlay = Map.empty; timeout = tout; playerForfeited = Set.empty; changeHandBool = false;}

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand

module Scrabble =
    
    let tileIdToChar (tileId: uint32 ) =  Convert.ToChar(tileId + 64u)
    
    let charToPointValue (tileId: uint32) =
        if tileId = 0u then 0
        else
            let c = tileIdToChar  tileId 
            match System.Char.ToUpper(c) with 
            | 'A' | 'E' | 'I' | 'O' | 'U' | 'L' | 'N' | 'S' | 'T' | 'R' -> 1
            | 'D' | 'G' -> 2
            | 'B' | 'C' | 'M' | 'P'  -> 3
            | 'F' | 'H' | 'V' | 'W' | 'Y' -> 4
            | 'K'-> 5
            | 'J' | 'X' -> 8
            | 'Q' | 'Z' -> 10
            | _ -> 0
        
    let charToTileId (c: char) = uint32(Char.ToUpper(c)) - 64u
    
    let tileIdToTile (tileId: uint32) (pieces: Map<uint32, tile>) =  Map.find tileId pieces 
    let stringToListOfChar (string: string) = List.ofArray (string.ToCharArray())
    
    let moveHorizontal = (1, 0)
    let moveVertical = (0, 1)
    
    let nextCoord (currentCoord: coord) (direction: coord) = ((fst currentCoord)+(fst direction), (snd currentCoord)+(snd direction))
    
    let previousCoord (currentCoord: coord) (direction: coord) = ((fst currentCoord)-(fst direction), (snd currentCoord)-(snd direction))
    
    let isSquareOccupied (square: coord) (piecesInPlay: Map<coord, uint32>) =
        match Map.tryFind square piecesInPlay with
        | Some occupantId -> true
        | None -> false
    
    let wordToSMMove (word: List<uint32 * char>) (startCoord: coord) (direction: coord) pieces =
         let rec aux (currentIndex: int) (coord: coord) =
             if currentIndex >= word.Length then [] else
             let char = snd word.[currentIndex]
             let tileId = fst word.[currentIndex]
             let move = (coord, (tileId, (char, charToPointValue(tileId))))
             let nextPosition = nextCoord coord direction
             move :: aux(currentIndex+1) (nextPosition)
         aux 0 startCoord
         
    
    let findWordsOnBoard (piecesInPlay: Map<coord, uint32>) (pieces: Map<uint32, tile>)=
        let rec concludeWord (wordBuilder: string) (currentCoord: coord) (direction: coord) =
            let nextPosition = nextCoord currentCoord direction
            if isSquareOccupied nextPosition piecesInPlay then 
                let tileIdOnNextPosition = Map.find nextPosition piecesInPlay
                concludeWord (wordBuilder + string (tileIdToChar tileIdOnNextPosition )) nextPosition direction
            else wordBuilder 
                    
        Map.fold ( fun acc coord tileId ->
            let charAtCoord = string (tileIdToChar tileId )
            let checkLeft = isSquareOccupied(previousCoord coord moveHorizontal) piecesInPlay
        
            let horizontalWord = if checkLeft then ("!", ((0,0), moveHorizontal)) else (concludeWord charAtCoord coord moveHorizontal, (coord, moveHorizontal)) //Left is free, finish word horizontally
            
            let checkUp = isSquareOccupied(previousCoord coord moveVertical) piecesInPlay
            
            let verticalWord = if checkUp then ("!", ((0,0), moveVertical)) else (concludeWord charAtCoord coord moveVertical, (coord, moveVertical)) //Up is free, finish word vertically
        
             
            let tempAcc = if (fst horizontalWord) = "!" then acc else horizontalWord :: acc
            if (fst verticalWord) = "!" then tempAcc else verticalWord :: tempAcc
            ) [] piecesInPlay

     
    let findPlayableWords (wordOnBoard: string) (startCoord: coord) (direction: coord) (st: State.state) (pieces: Map<uint32, tile>) =
        let subdict =
           List.fold (fun acc char -> 
               match Dictionary.step char acc with 
                 | Some (_, node) -> node
                 | None -> st.dict  
           ) st.dict (stringToListOfChar wordOnBoard)
           
        let rec aux (hand: MultiSet.MultiSet<uint32>) (subdict: Dictionary.Dict) (currentWord: List<uint32 * char>) =    
            List.fold (fun acc tileId -> 
                let charPointvalueSet = tileIdToTile tileId pieces
                Set.fold (fun acc (char, pv) -> 
                   match Dictionary.step char subdict with 
                     | Some (b, node) ->
                        let newHand = MultiSet.removeSingle tileId hand 
                        if b then 
                            ((currentWord @ [(tileId, char)]), (startCoord, direction)) :: aux newHand node (currentWord @ [(tileId, char)]) 
                        else
                            aux newHand node (currentWord @ [(tileId, char)]) 
                     | None -> acc
                ) acc charPointvalueSet
            ) [] (MultiSet.toList hand)
        
        List.map (fun char -> ((charToTileId char), char)) (stringToListOfChar wordOnBoard) 
        |> aux st.hand subdict 
    
    let findLongestWord (wordList: List<List<uint32 * char> * (coord * coord)>) =
        List.fold (fun acc (wordList: List<uint32 * char>, (coord, direction)) ->
           if List.length wordList > List.length (fst acc) then (wordList, (coord, direction)) else acc
            ) ([], ((0, 0), moveHorizontal))  wordList
    
    let validateBoard (piecesInPlay: Map<coord, uint32>) (st: State.state) pieces =
            let someWords = findWordsOnBoard piecesInPlay pieces
            someWords
            |> List.filter (fun word -> (fst word).Length > 1)
            |> List.fold (fun acc word -> if not (Dictionary.lookup (fst word) st.dict) then false else acc) true
            
    let temporaryState (piecesInPlay: Map<coord, uint32>) (word: (List<uint32 * char>), (coord, direction)) =
        let rec aux (currentIndex: int) (coord: coord) (pip :Map<coord, uint32>) = 
            if currentIndex >= word.Length then pip
            else
                let newMap = Map.add coord (fst (word.[currentIndex])) pip
                let nextPosition = nextCoord coord direction 
                aux (currentIndex+1) nextPosition newMap
        aux 0 coord piecesInPlay       
                           
    let findPlaceableWord (st : State.state) (pieces: Map<uint32, tile>) =
        let wordsOnBoard1 = findWordsOnBoard st.piecesInPlay pieces
        let wordsOnBoard2 = if wordsOnBoard1.Length = 0 then [("",((0,0),(0,1)))] else wordsOnBoard1
        let allPlaceableWords = 
            List.fold (fun acc (word, (coord, direction)) -> 
                let playableWords = findPlayableWords word coord direction st pieces
                let placeableWordsFromWordOnBoard = 
                    List.fold (fun acc ((wordList: List<uint32 * char>), (coord, direction)) -> 
                        let rec aux (currentIndex: int) (coord: coord) (pip :Map<coord, uint32>)= 
                            if currentIndex >= word.Length then
                                let filteredBoard = List.filter (fun ((wordOnBoard:string), _ ) -> wordOnBoard.Length > 1) (findWordsOnBoard pip pieces)
                                List.fold (fun acc (wordOnBoard: string, _) -> 
                                        if Dictionary.lookup wordOnBoard st.dict then acc else false 
                                        ) true filteredBoard 
                            else 
                                let tmpPIP = Map.add coord (fst (wordList.[currentIndex])) pip 
                                let nextPosition = nextCoord coord direction  
                                aux(currentIndex+1) nextPosition tmpPIP
                        if aux 0 coord st.piecesInPlay then  (wordList,(coord, direction)) :: acc 
                            else acc 
                    ) [] playableWords
                placeableWordsFromWordOnBoard @ acc                   
            ) [] wordsOnBoard2
        let validWords: (List<(List<uint32 * char>) * (coord * coord)>)  =
            List.fold (fun acc word ->
                if validateBoard (temporaryState st.piecesInPlay word) st pieces then
                    word :: acc
                else
                    acc
            ) [] allPlaceableWords 
        findLongestWord validWords  
    
    let movesToListOfPlays (move : list<coord * (uint32 * (char * int))>) : list<coord*uint32> = 
        let mutable newList = []
        for element in move do
            newList <- ((fst element), (fst (snd element))) :: newList
        newList 

    let movesToListOfTiles (move : list<coord * (uint32 * (char * int))>) : list<uint32> =
        let mutable newList = []
        for element in move do
            newList <- (fst (snd element)) :: newList
        newList  

    let nextTurn (st : State.state) = 
        { st with 
            playerTurn = (st.playerTurn % st.playerAmount) + 1u;}

    let removePlayedPiecesFromHand (st : State.state) (move : list<coord * (uint32 * (char * int))>) =
        List.fold (fun acc x -> MultiSet.removeSingle x acc) st.hand (movesToListOfTiles move)

    let updateHandWithNewPieces (st : State.state) (np : list<uint32*uint32>) (move : list<coord * (uint32 * (char * int))>) = 
        { st with
            hand = List.fold (fun acc (x, k) -> MultiSet.add x k acc) (removePlayedPiecesFromHand st move) np; }

    let updatePlayerCount (st : State.state) (pid : uint32) = 
        { st with
            playerForfeited = st.playerForfeited.Add(pid);}
    
    let filterMove (st : State.state) (move : list<coord * (uint32 * (char * int))>) =
       List.filter (fun (coord, _) -> not (isSquareOccupied coord st.piecesInPlay)) move
    
    let changeAllTilesInHand (st : State.state) (tiles : list<uint32 * uint32>) =
        { st with
            hand = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty tiles;
            changeHandBool = false; }

    let updateBoolAfterPass (st : State.state) =
        { st with
            changeHandBool = true;}
      
    let playGame cstream (pieces: Map<uint32, tile>) (st : State.state) =
        let updatePiecesInPlay (st : State.state) (move : list<coord * (uint32 * (char * int))>) =
            { st with
                piecesInPlay = List.fold (fun acc (coord,(tileId,(char, _))) -> Map.add coord (charToTileId char) acc) st.piecesInPlay move; }
        
        let rec aux (st : State.state) =
            
            match st.playerForfeited.Contains(st.playerTurn) with
            | true -> 
                let st' = nextTurn st
                aux st'
            | false ->
                match st.playerTurn = st.playerNumber with
                | true ->
                    let placeableWord = findPlaceableWord st pieces
                    if (fst placeableWord).Length > 1 then
                        let move = filterMove st (wordToSMMove (fst placeableWord) (fst (snd placeableWord)) (snd (snd placeableWord)) pieces)
                        debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                        send cstream (SMPlay move)
                    else
                        debugPrint (sprintf "Player %d -> Server:\npassed\n" (State.playerNumber st)) // keep the debug lines. They are useful.
                        if st.changeHandBool then
                            send cstream (SMChange (MultiSet.toList st.hand))
                        else 
                            send cstream (SMPass)       
                | false -> 
                    debugPrint (sprintf "Waiting for my turn")
            
            let msg = recv cstream

            match msg with
            | RCM (CMPlaySuccess(move, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                debugPrint (sprintf "Player %d <- Server:\n%A\nsuccess\n" (State.playerNumber st) move)
                let st' = updatePiecesInPlay st move
                let st'' = updateHandWithNewPieces st' newPieces move
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
            | RCM (CMPassed (pid)) ->
                (* Player passed. Update your state *)
                debugPrint (sprintf "Player %d <- Server:\npassed\n" (pid))
                let st' = nextTurn st
                if pid = st'.playerNumber then
                    let st'' = updateBoolAfterPass st'
                    aux st''
                else
                    aux st'
            | RCM (CMTimeout (pid)) ->
                (* Player timed out. Update your state *)
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
                let st' = changeAllTilesInHand st tiles
                let st'' = nextTurn st'
                aux st''
            | RCM (CMGameOver _) -> () //Loop ends i.e. Game ends.
            | RGPE err -> 
                debugPrint (sprintf "Gameplay Error:\n%A" err)
                aux st

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
        
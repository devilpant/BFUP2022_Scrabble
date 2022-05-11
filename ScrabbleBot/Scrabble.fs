namespace Huey

open System
open Eval
open Parser
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
    }

    let mkState b d pn h pa pt = {board = b; dict = d;  playerNumber = pn; hand = h; playerAmount = pa; playerTurn = pt; piecesInPlay = Map.empty;}

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
    let stringToSMMove (word: string) (startCoord: coord) (direction: coord) =
         let rec aux (currentIndex: int) (coord: coord) =
             if currentIndex >= word.Length then [] else
             let char = word.[currentIndex]
             let tileId = charToTileId(char)
             let move = (coord, (tileId, (char, charToPointValue(char))))
             let nextPosition = nextCoord coord direction
             move :: aux(currentIndex+1) (nextPosition)
         aux 0 startCoord 
    
    
    //Function used in findWordsOnBoard to finalize words from the current coordinate of a starter character.
    
        
    let findWordsOnBoard (piecesInPlay: Map<coord, uint32>) =
        let rec concludeWord (wordBuilder: string) (currentCoord: coord) (direction: coord) =
            let nextPosition = nextCoord currentCoord direction
            if isSquareOccupied nextPosition piecesInPlay then
                let tileIdOnNextPosition = Map.find nextPosition piecesInPlay
                concludeWord (wordBuilder + string (tileIdToChar tileIdOnNextPosition)) nextPosition direction
            else wordBuilder
                    
        Map.fold ( fun acc coord tileId ->
            let charAtCoord = string (tileIdToChar tileId)
            let checkLeft = isSquareOccupied(previousCoord coord moveHorizontal) piecesInPlay
            //Dummy value, since there is something on the left hand side of current character
            let horizontalWord = if checkLeft then ("!", ((0,0), moveHorizontal)) else (concludeWord charAtCoord coord moveHorizontal, (coord, moveHorizontal))
            
            let checkUp = isSquareOccupied(previousCoord coord moveVertical) piecesInPlay
            //Dummy value, since there is something above the current character
            let verticalWord = if checkUp then ("!", ((0,0), moveVertical)) else (concludeWord charAtCoord coord moveVertical, (coord, moveVertical))
        
            //If the character at the current coordinate BEGINS a word, then prepend it to the list of words already on the board. 
            let tempAcc = if (fst horizontalWord) = "!" then acc else horizontalWord :: acc
            if (fst verticalWord) = "!" then tempAcc else verticalWord :: tempAcc
            ) [] piecesInPlay
    
    let testHand =
         MultiSet.empty
         |> MultiSet.add (charToTileId 'a') 2u
         |> MultiSet.add (charToTileId 's') 1u
    
         
    // MultiSet.fold f acc testHand -> 
         
    //let rec findMove (tiles: Map<uint32, tile>) (st:  State.state) =
        
        
       
    // Handle coordinates (x,y) stuff like horizontal and vertical direction of the word. Note that (0,0) is center.
    // Therefore (-1, 0) is hor left, and (0, -1) is ver up. (0,1) ver down. (1,0) hor right
    
    let playGame cstream (pieces: Map<uint32, tile>) (st : State.state) =

        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)

            // remove the force print when you move on from manual input (or when you have learnt the format)
            forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            let input =  System.Console.ReadLine()
            let move = failwith "notImplemented" //move = findMove move

            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            send cstream (SMPlay move)

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = st // This state needs to be updated
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

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet numPlayers playerTurn )
        
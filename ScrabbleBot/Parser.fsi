module internal Parser
    
    open ScrabbleUtil
    
    type word   = (char * int) list
    type square = Map<int, word -> int -> int -> int>
    type boardFun = coord -> square option
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun
    }

    val parseBoardProg : boardProg -> board
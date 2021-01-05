package tictactoe

import tictactoe.SimpleTypes._

object CompoundTypes {

    final case class Position(column: OneThroughThree, row: OneThroughThree)

    final case class Move(at: Position, place: Letter)

    final case class GameState(board: Board,  whoseTurn: Letter)


}

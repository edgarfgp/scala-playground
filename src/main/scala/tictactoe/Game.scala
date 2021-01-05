package tictactoe

import tictactoe.CompoundTypes._
import tictactoe.SimpleTypes.Letter._
import tictactoe.SimpleTypes.OneThroughThree._
import tictactoe.SimpleTypes.OutCome._
import tictactoe.SimpleTypes.Value._
import tictactoe.SimpleTypes._

import scala.annotation.tailrec

object Game {

    val waysToWin = List(
        (Position(One, One), Position(Two, One), Position(Three, One)),
        (Position(Two, One), Position(Two, Two), Position(Two, Three)),
        (Position(Three, One), Position(Three, Two), Position(Three, Three)),

        (Position(One, One), Position(Two, One), Position(Three, One)),
        (Position(One, Two), Position(Two, Two), Position(Three, Two)),
        (Position(One, Three), Position(Two, Three), Position(Three, Three)),

        (Position(One, One), Position(Two, Two), Position(Three, Three)),
        (Position(One, Three), Position(Two, Two), Position(Three, One)),
    )

    def select(board: Board)(position: Position): Value = {
        (board, position) match {
            case (((x, _, _), _, _), Position(One, One)) => x
            case (((_, x, _), _, _), Position(One, Two)) => x
            case (((_, _, x), _, _), Position(One, Three)) => x
            case ((_, (x, _, _), _), Position(Two, One)) => x
            case ((_, (_, x, _), _), Position(Two, Two)) => x
            case ((_, (_, _, x), _), Position(Two, Three)) => x
            case ((_, _, (x, _, _)), Position(Three, One)) => x
            case ((_, _, (_, x, _)), Position(Three, Two)) => x
            case ((_, _, (_, _, x)), Position(Three, Three)) => x
        }
    }

    def set(value: Value)(board: Board)(position: Position): Board = {
        (board, position) match {
            case (((_, v2, v3), r2, r3), Position(One, One)) => ((value, v2, v3), r2, r3)
            case (((v1, _, v3), r2, r3), Position(One, Two)) => ((v1, value, v3), r2, r3)
            case (((v1, v2, _), r2, r3), Position(One, Three)) => ((v1, v2, value), r2, r3)
            case ((r1, (_, v2, v3), r3), Position(Two, One)) => (r1, (value, v2, v3), r3)
            case ((r1, (v1, _, v3), r3), Position(Two, Two)) => (r1, (v1, value, v3), r3)
            case ((r1, (v1, v2, _), r3), Position(Two, Three)) => (r1, (v1, v2, value), r3)
            case ((r1, r2, (_, v2, v3)), Position(Three, One)) => (r1, r2, (value, v2, v3))
            case ((r1, r2, (v1, _, v3)), Position(Three, Two)) => (r1, r2, (v1, value, v3))
            case ((r1, r2, (v1, v2, _)), Position(Three, Three)) => (r1, r2, (v1, v2, value))
        }
    }

    def modify(f: Value => Value)(board: Board)(position: Position): BoardResult = {
        set(f(select(board)(position)))(board)(position)
    }

    def placePieceIfCan(piece: Letter)(board: Board)(position: Position): BoardResult = {
        modify {
            case Unspecified => Specified(piece)
            case Specified(letter) => Specified(letter)
        }(board)(position)
    }

    def makeMove(board: Board)(move: Move): Option[EmptyBoard] = {
        if (select(board)(move.at) == Unspecified)
            Some(placePieceIfCan(move.place)(board)(move.at))
        else None
    }

    def map3[A](f: Position => Value)(a: Position, b: Position, c: Position): (Value, Value, Value) = (f(a), f(b), f(c))

    def winner(board: Board): Option[Letter] = {
        val winPaths = waysToWin.map {
            case (p1, _, _) => map3(value => select(board)(value))(p1, _, _)
            case (_, p2, _) => map3(value => select(board)(value))(_, p2, _)
            case (_, _, p3) => map3(value => select(board)(value))(_, _, p3)
        }

        if (winPaths.contains((X, X, X))) {
            Some(X)
        } else if (winPaths.contains(O, O, O)) {
            Some(O)
        } else {
            None
        }
    }

    def slotsRemaining(board: Board): Boolean = {
        val cells = for {
            res <- List(One, Two, Three).flatMap(column => List(One, Two, Three).map(row => Position(column, row)))
        } yield res

        cells.exists(position => { select(board)(position) == Unspecified })
    }

    def outCome(board: Board): OutCome = {
        (winner(board), slotsRemaining(board)) match {
            case (Some(winningLetter), _) => Winner(winningLetter)
            case (None, false) => Draw
            case _ => NoneYet
        }
    }

    def renderValue(value: Value): String = {
        value match {
            case Unspecified => " "
            case Specified(letter) =>
                letter match {
                    case X => "X"
                    case O => "O"
                }
        }
    }

    def otherPlayer(letter: Letter): Letter =
        letter match {
            case X => O
            case O => X
        }

    // FIXME Find a better way of handling this
    def render(value: EmptyBoard): String = {
        val va = renderValue(value._1._1)
        val vb = renderValue(value._1._2)
        val vc = renderValue(value._1._3)
        val vd = renderValue(value._2._1)
        val ve = renderValue(value._2._2)
        val vf = renderValue(value._2._3)
        val vg = renderValue(value._3._1)
        val vh = renderValue(value._3._2)
        val vi = renderValue(value._3._3)
        s"""
         $va|$vb|$vc
         _ _ _
         $vd|$ve|$vf
         _ _ _
         $vg|$vh|$vi""".stripIndent()
    }


    def parseOneThroughThree(raw: String): Option[OneThroughThree] = {
        raw match {
            case "1" => Some(One)
            case "2" => Some(Two)
            case "3" => Some(Three)
            case _ => None
        }
    }

    def parseMove(raw: String): Option[Position] = {
        raw.split(" ") match {
            case Array(r, c) =>
                (parseOneThroughThree(r), parseOneThroughThree(c)) match {
                    case (Some(row), Some(column)) => Some(Position(column, row))
                    case _ => None
                }
            case _ => None
        }
    }

    @tailrec
    def readMoveIo(letter: Letter): Move = {
        val readLine = scala.io.StdIn.readLine()
        parseMove(readLine) match {
            case Some(position) => Move(position, letter)
            case None =>
                println("Bad move! Please input row and column numbers")
                readMoveIo(letter)
        }
    }

    @tailrec
    def nextMoveIo(board: (Row, Row, Row), letter: Letter): BoardResult = {
        val move = readMoveIo(letter)
        makeMove(board)(move) match {
            case Some(newBoard) => newBoard
            case None =>
                println("Bad move! Position is occupied.")
                nextMoveIo(board, letter)
        }
    }

    @tailrec
    def playIo(gameState: GameState): Unit = {
        println(s"${gameState.whoseTurn} Turn")
        val board = render(gameState.board)
        println(s"$board")
        println("")
        val newBoard = nextMoveIo(gameState.board, gameState.whoseTurn)
        println("")
        outCome(newBoard) match {
            case Winner(letter) =>
                println(s"wins!!! $letter")
                println {
                    render(newBoard)
                }
            case OutCome.Draw =>
                println("It's a draw!")
            case OutCome.NoneYet =>
                playIo(GameState(newBoard, otherPlayer(gameState.whoseTurn)))
        }
    }

    def main(args: Array[String]): Unit = {

        val emptyBoard = (
            (Unspecified, Unspecified, Unspecified),
            (Unspecified, Unspecified, Unspecified),
            (Unspecified, Unspecified, Unspecified))

        playIo(GameState(board = emptyBoard, whoseTurn = X))
    }
}
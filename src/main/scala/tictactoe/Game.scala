package tictactoe

import tictactoe.CompoundTypes._
import tictactoe.SimpleTypes.Letter._
import tictactoe.SimpleTypes.OneThroughThree._
import tictactoe.SimpleTypes.OutCome._
import tictactoe.SimpleTypes.Value._
import tictactoe.SimpleTypes._

import scala.annotation.tailrec

object Game {

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

    def winner(board: Board): Option[Letter] = {
        val waysToWin = List(
            (Position(row = One, column = One), Position(row = One, column = Two), Position(row = One, column = Three)),
            (Position(row = Two, column=  One), Position(row = Two,column =  Two), Position(row = Two,column =  Three)),
            (Position(row = Three,column = One), Position(row = Three,column = Two), Position(row = Three, column= Three)),

            (Position(row = One,column =  One), Position(row = Two,column =  One), Position(row = Three,column =  One)),
            (Position(row = One,column =  Two), Position(row = Two,column =  Two), Position(row = Three,column =  Two)),
            (Position(row = One,column =  Three), Position(row = Two,column =  Three), Position(row = Three,column =  Three)),

            (Position(row =One,column = One), Position(row =Two,column = Two), Position(row =Three,column = Three)),
            (Position(row =One,column = Three), Position(row =Two,column = Two), Position(row =Three, column = One)),
        )

        val winPaths = waysToWin.map { case (position0, position1, position2) =>
            select(board)(position0)
            select(board)(position1)
            select(board)(position2)
        }

        if (winPaths.contains(Value.Specified(X))) {
            Some(X)
        } else if (winPaths.contains(Value.Specified(O))) {
            Some(O)
        } else {
            None
        }
    }

    def slotsRemaining(board: Board): Boolean = {
        val cells = for {
            row <- List(One, Two, Three)
            column <- List(One, Two, Three)
        } yield Position(row = row, column = column)

        cells.exists(position => { select(board)(position) == Unspecified })
    }

    def outCome(board: Board): OutCome = {
        (winner(board), slotsRemaining(board)) match {
            case (Some(winningLetter), _) =>
                Winner(winningLetter)
            case (None, false) =>
                Draw
            case _ =>
                NoneYet
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

    def render(value: EmptyBoard): String = {
        val a = renderValue(value._1._1)
        val b = renderValue(value._2._1)
        val c = renderValue(value._3._1)

        val d = renderValue(value._1._2)
        val e = renderValue(value._2._2)
        val f = renderValue(value._3._2)

        val g = renderValue(value._1._3)
        val h = renderValue(value._2._3)
        val i = renderValue(value._3._3)
        s"""
         $a|$b|$c
         _ _ _
         $d|$e|$f
         _ _ _
         $g|$h|$i""".stripIndent()
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
        val turn = gameState.whoseTurn
        println(s"$turn Turn")
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
package tictactoe

object SimpleTypes {

    sealed trait OutCome

    object OutCome {
        final object  NoneYet extends OutCome

        final case class Winner(letter: Letter) extends OutCome

        final object Draw extends OutCome
    }

    sealed trait Letter
    object Letter {
        final object X extends Letter

        final object O extends Letter
    }

    sealed trait Value

    object Value {
        final object Unspecified extends Value

        final case class Specified(letter: Letter) extends Value
    }

    sealed trait OneThroughThree

    object OneThroughThree {
        final object One extends OneThroughThree
        final object Two extends OneThroughThree
        final object Three extends OneThroughThree
    }

    type Row = (Value, Value, Value)

    type Board = (Row, Row, Row)

    type EmptyBoard = ((Value, Value, Value) , (Value, Value, Value), (Value, Value, Value))

    type BoardResult = ((Value, Value, Value), Row, Row)

}

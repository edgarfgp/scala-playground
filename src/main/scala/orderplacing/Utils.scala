package orderplacing

object Utils {

    def defaultIfNone[A](defaultValue: A, option: Option[A]) : A = option match {
        case Some(value) => value
        case None => defaultValue
    }

    def listOfOption[A](option: Option[A]) : List[A] = option match {
        case Some(value) => List(value)
        case None =>List()
    }
}

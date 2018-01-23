package befunge

object stack {

  case class Stack[A](state: List[A]) {
    def push(a: A): Stack[A] =
      Stack(a +: state)

    def pop: (Option[A], Stack[A]) =
      state match {
        case Nil => Option.empty[A] -> this
        case x :: xs => Some(x) -> Stack(xs)
      }
  }

  object Stack {
    def empty[A]: Stack[A] = Stack(List.empty[A])
  }
}

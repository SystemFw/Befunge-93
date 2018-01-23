package befunge

object primitives {

  trait Stack[F[_], A] {
    def pop: F[A]
    def push(a: A): F[Unit]
  }

  trait Motion[F[_]] {
    def advance: F[Unit]
    def changeDirection(d: motion.Direction): F[Unit]
  }

  trait Console[F[_]] {
    def put(a: String): F[Unit]
  }

}

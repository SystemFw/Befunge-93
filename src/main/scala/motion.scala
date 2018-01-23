package befunge

import cats.Monoid
import cats.implicits._

object motion {
  case class Width(v: Int)
  case class Height(v: Int)

  case class Point(x: Int, y: Int)
  object Point {
    implicit def points: Monoid[Point] = new Monoid[Point] {
      def combine(p1: Point, p2: Point) = Point(p1.x + p2.x, p1.y + p2.y)
      def empty: Point = Point(0, 0)
    }
  }

  case class Torus[A](w: Width, h: Height, space: Map[Point, A], focus: Point) {
    private def wrapAround: Point => Point =
      p =>
        p.copy(
          x = if (p.x >= 0) p.x % w.v else w.v + p.x,
          y = if (p.y >= 0) p.y % h.v else h.v + p.y
      )

    def get: Option[A] = space.get(focus)

    def advance(d: Direction): Torus[A] = {
      val newFocus = focus |+| d.toPoint
      this.copy(focus = wrapAround(newFocus))
    }

    def l = advance(Left)
    def r = advance(Right)
    def u = advance(Up)
    def d = advance(Down)
  }

  object Torus {
    def empty[A]: Torus[A] =
      Torus(Width(80), Height(25), Map.empty[Point, A], Point(0, 0))

    def fromString[A](s: String, f: Char => A): Torus[A] = {
      val lines = s.split("""\R+""").toVector
      val points =
        lines.zipWithIndex
          .flatMap {
            case (s, y) =>
              s.toVector.zipWithIndex.map {
                case (c, x) =>
                  Point(x, y) -> c
              }
          }
          .toMap
          .mapValues(f)

      val y = Height(lines.length)
      val x = Width(lines.map(_.length).max)

      Torus(x, y, points, focus = Point(0, 0))
    }
  }

  sealed trait Direction {
    def toPoint: Point = this match {
      case Up => Point(0, -1)
      case Down => Point(0, 1)
      case Left => Point(-1, 0)
      case Right => Point(1, 0)
    }
  }
  case object Up extends Direction
  case object Down extends Direction
  case object Left extends Direction
  case object Right extends Direction
}

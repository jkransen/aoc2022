import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source
import scala.math.abs
import scala.math.max
import scala.math.min

object Day12 extends App {

  val heights: List[List[Char]] = Source.fromResource("day12_1").getLines().map(_.toList).toList
  val height = heights.size
  val width = heights.head.size

  case class Point(x: Int, y: Int, height: Char, distance: Int)

  val end: Point = {
    val matches = for {
      y <- 0.until(height)
      x <- 0.until(width)
      if (heights(y)(x) == 'E')
    } yield Point(x, y, 'z', 0)
    matches.head
  }

  val distance = findShortestPath(Queue(end))
  println(s"1. Distance $distance")

  @tailrec
  def findShortestPath(heads: Queue[Point], visited: Set[(Int, Int)] = Set()): Int = {
    val head = heads.head
    if (head.height == 'S') {
      head.distance
    } else {
      val reachable = getReachable(head).filterNot(p => visited.contains((p.x, p.y)))
      findShortestPath(heads.tail ++ reachable, visited + ((head.x, head.y)))
    }
  }

  def getReachable(from: Point): Set[Point] = {
    val points = for {
      x <- (max(0, from.x - 1)).to(min(from.x + 1, width - 1))
      y <- (max(0, from.y - 1)).to(min(from.y + 1, height - 1))
      if (x == from.x ^ y == from.y)
      ch: Char = heights(y)(x)
      if (ch != 'E')
      if (abs(ch.toInt - from.height.toInt) <= 1) || (ch == 'S' && (from.height == 'a' || from.height == 'b'))
    } yield Point(x, y, ch, from.distance + 1)
    points.toSet
  }

}

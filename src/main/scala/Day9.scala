import java.lang.Math.abs
import scala.annotation.tailrec
import scala.io.Source

object Day9 extends App {

  val lines: List[String] = Source.fromResource("day9_1").getLines().toList
  val moveRegex = "^([UDLR])\\s(\\d+)$".r.pattern

  case class Move(direction: Char, amount: Int)

  case class Knot(x: Int = 0, y: Int = 0) {
    def follow(head: Knot): Knot = {
      val dx = head.x - x
      val dy = head.y - y
      if (abs(dx) <= 1 && abs(dy) <= 1) {
        this
      } else {
        this.copy(x = x + dx.signum, y = y + dy.signum)
      }
    }
  }

  val moves: List[Move] = scanMoves(lines)
  println("Moves: " + moves)

  val twoknots = List(Knot(), Knot())
  val visited = makeMoves(moves, twoknots)
  println(s"1. Number of visited by tail: ${visited.size}")

  val tenknots = 1.to(10).map(_ => Knot()).toList
  val visited2 = makeMoves(moves, tenknots)
  println(s"2. Number of visited by tail with 10 knots: ${visited2.size}")

  @tailrec
  def makeMoves(moves: List[Move], knots: List[Knot], visited: Set[Knot] = Set()): Set[Knot] = {
    if (moves.isEmpty) {
      visited
    } else {
      val move = moves.head
      val (newKnots, newVisited) = makeMove(knots, move, visited)
      makeMoves(moves.tail, newKnots, newVisited)
    }
  }

  @tailrec
  def makeMove(knots: List[Knot], move: Move, visited: Set[Knot]): (List[Knot], Set[Knot]) = {
    if (move.amount == 0) {
      (knots, visited)
    } else {
      val head = knots.head
      val newHead = move.direction match {
        case 'U' => head.copy(y = head.y - 1)
        case 'D' => head.copy(y = head.y + 1)
        case 'L' => head.copy(x = head.x - 1)
        case 'R' => head.copy(x = head.x + 1)
      }
      val newKnots = newHead :: follow(knots.tail, newHead)
      val newVisited = visited + newKnots.last
      makeMove(newKnots, move.copy(amount = move.amount - 1), newVisited)
    }
  }

  @tailrec
  def follow(knots: List[Knot], head: Knot, aggregate: List[Knot] = List()): List[Knot] = {
    if (knots.isEmpty) {
      aggregate
    } else {
      val newHead = knots.head.follow(head)
      follow(knots.tail, newHead, aggregate :+ newHead)
    }
  }

  @tailrec
  def scanMoves(input: List[String], aggregate: List[Move] = List.empty[Move]): List[Move] = {
    if (input.isEmpty) {
      aggregate
    } else {
      val matcher = moveRegex.matcher(input.head)
      if (matcher.matches()) {
        val move = Move(matcher.group(1).head, matcher.group(2).toInt)
        scanMoves(input.tail, aggregate :+ move)
      } else {
        scanMoves(input.tail, aggregate)
      }
    }
  }

  assert(visited.size == 6269)
  assert(visited2.size == 2557)
}

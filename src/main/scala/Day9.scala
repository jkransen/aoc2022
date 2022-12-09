import java.lang.Math.abs
import scala.annotation.tailrec
import scala.io.Source

object Day9 extends App {

  val lines: List[String] = Source.fromResource("day9_1").getLines().toList
  val moveRegex = "^([UDLR])\\s(\\d+)$".r.pattern

  case class Move(direction: Char, amount: Int)

  case class Head(x: Int = 0, y: Int = 0)

  case class Tail(x: Int = 0, y: Int = 0) {
    def follow(head: Head): Tail = {
      val dx = head.x - x
      val dy = head.y - y
      if (abs(dx) <= 1 && abs(dy) <= 1) {
        this
      } else if (dx == 2) {
        if (dy == 0) {
          this.copy(x = x + 1)
        } else {
          this.copy(x = x + 1, y = y + dy)
        }
      } else if (dx == -2) {
        if (dy == 0) {
          this.copy(x = x - 1)
        } else {
          this.copy(x = x - 1, y = y + dy)
        }
      } else if (dy == 2) {
        if (dx == 0) {
          this.copy(y = y + 1)
        } else {
          this.copy(x = x + dx, y = y + 1)
        }
      } else { // if (dy == -2)
        if (dx == 0) {
          this.copy(y = y - 1)
        } else {
          this.copy(x = x + dx, y = y - 1)
        }
      }
    }
  }

  val moves: List[Move] = scanMoves(lines)
  println("Moves: " + moves)

  val visited = makeMoves(moves)
  println(s"1. Number of visited by tail: ${visited.size}")

  @tailrec
  def makeMoves(moves: List[Move], head: Head = Head(), tail: Tail = Tail(), visited: Set[Tail] = Set()): Set[Tail] = {
    if (moves.isEmpty) {
      visited
    } else {
      val move = moves.head
      val (newHead, newTail, newVisited) = makeMove(head, tail, move, visited)
      makeMoves(moves.tail, newHead, newTail, newVisited)
    }
  }

  @tailrec
  def makeMove(head: Head, tail: Tail, move: Move, visited: Set[Tail]): (Head, Tail, Set[Tail]) = {
    if (move.amount == 0) {
      (head, tail, visited)
    } else {
      val newHead = move.direction match {
        case 'U' => head.copy(y = head.y - 1)
        case 'D' => head.copy(y = head.y + 1)
        case 'L' => head.copy(x = head.x - 1)
        case 'R' => head.copy(x = head.x + 1)
      }
      val newTail = tail.follow(newHead)
      makeMove(newHead, newTail, move.copy(amount = move.amount - 1), visited + newTail)
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
}

import scala.annotation.tailrec
import scala.io.Source

object Day5 extends App {

  case class Move(amount: Int, from: Int, to: Int)

  val bottomRegex = "^[\\d\\s]+$".r.pattern
  val moveRegex = "move (\\d+) from (\\d+) to (\\d+)".r.pattern

  val lines = Source.fromResource("day5_1").getLines().toList
  val moves = parseMoves(lines)
  val rows = parseRows(lines)
  val stacks = rotate(rows)

  val stacksAfterMoves = makeMoves(stacks, moves, crateMover9000)
  val cratesOnTop = stacksAfterMoves.flatMap(_.headOption).mkString
  println(s"1. CrateMover 9000: $cratesOnTop")

  val stacksAfterMoves2 = makeMoves(stacks, moves, crateMover9001)
  val cratesOnTop2 = stacksAfterMoves2.flatMap(_.headOption).mkString
  println(s"2. CrateMover 9001: $cratesOnTop2")

  @tailrec
  def makeMoves(stacks: List[List[Char]], moves: List[Move], moveOperation: (List[List[Char]], Move) => List[List[Char]]): List[List[Char]] = {
    if (moves.isEmpty) {
      stacks
    } else {
      val nextMove = moves.head
      val updatedStacks = moveOperation(stacks, nextMove)
      makeMoves(updatedStacks, moves.tail, moveOperation)
    }
  }

  @tailrec
  def crateMover9000(stacks: List[List[Char]], move: Move): List[List[Char]] = {
    if (move.amount == 0) {
      stacks
    } else {
      val updated = stacks
        .updated(move.to, List(stacks(move.from).head) ++ stacks(move.to))
        .updated(move.from, stacks(move.from).tail)
      crateMover9000(updated, move.copy(amount = move.amount - 1))
    }
  }

  def crateMover9001(stacks: List[List[Char]], move: Move): List[List[Char]] = {
    val (upper, lower) = stacks(move.from).splitAt(move.amount)
    stacks.updated(move.to, upper ++ stacks(move.to))
      .updated(move.from, lower)
  }

  @tailrec
  def rotate(stack: List[List[Char]], rotated: List[List[Char]] = List()): List[List[Char]] = {
    if (stack.isEmpty) {
      rotated
    } else {
      val topRow = stack.head
      val newRotated = topRow.zipAll(rotated, List(), List()).map {
        case (' ', a) => a
        case (n: Char, a) => List(n) ++ a
      }
      rotate(stack.tail, newRotated)
    }
  }

  @tailrec
  def parseMoves(input: List[String], moves: List[Move] = List()): List[Move] = {
    if (input.isEmpty) {
      moves
    } else {
      parseMove(input.head) match {
        case Some(move) => parseMoves(input.tail, moves ++ List(move))
        case None => parseMoves(input.tail, moves)
      }
    }
  }

  def parseMove(line: String): Option[Move] = {
    val matcher = moveRegex.matcher(line)
    if (matcher.matches()) {
      Some(Move(matcher.group(1).toInt, matcher.group(2).toInt - 1, matcher.group(3).toInt - 1))
    } else {
      None
    }
  }

  @tailrec
  def parseRows(input: List[String], stack: List[List[Char]] = List()): List[List[Char]] = {
    val line = input.head
    if (bottomRegex.matcher(line).matches()) {
      stack
    } else {
      val row = for {
        i <- 1.to((line.length + 1) / 4)
      } yield line(4 * (i - 1) + 1)

      parseRows(input.tail, List(row.toList) ++ stack)
    }
  }

  assert(cratesOnTop.equals("VRWBSFZWM"))
  assert(cratesOnTop2.equals("RBTWJWMCF"))
}

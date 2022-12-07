import scala.annotation.tailrec
import scala.io.Source

object Day5 extends App {

  case class Move(amount: Int, from: Int, to: Int)
  type Stack = List[Char]

  val bottomRegex = "^[\\d\\s]+$".r.pattern
  val moveRegex = "move (\\d+) from (\\d+) to (\\d+)".r.pattern

  val lines: List[String] = Source.fromResource("day5_1").getLines().toList
  val moves: List[Move] = parseMoves(lines)
  val rows: List[List[Char]] = parseRows(lines)
  val stacks: List[Stack] = rotate(rows)

  val stacksAfterMoves: List[Stack] = makeMoves(stacks, moves, crateMover9000)
  val cratesOnTop = stacksAfterMoves.flatMap(_.headOption).mkString
  println(s"1. CrateMover 9000: $cratesOnTop")

  val stacksAfterMoves2: List[Stack] = makeMoves(stacks, moves, crateMover9001)
  val cratesOnTop2 = stacksAfterMoves2.flatMap(_.headOption).mkString
  println(s"2. CrateMover 9001: $cratesOnTop2")

  @tailrec
  def makeMoves(stacks: List[Stack], moves: List[Move], crateMover: (List[Stack], Move) => List[Stack]): List[Stack] = {
    if (moves.isEmpty) {
      stacks
    } else {
      val nextMove = moves.head
      val updatedStacks = crateMover(stacks, nextMove)
      makeMoves(updatedStacks, moves.tail, crateMover)
    }
  }

  @tailrec
  def crateMover9000(stacks: List[Stack], move: Move): List[Stack] = {
    if (move.amount == 0) {
      stacks
    } else {
      val updated = stacks
        .updated(move.to, stacks(move.from).head :: stacks(move.to))
        .updated(move.from, stacks(move.from).tail)
      crateMover9000(updated, move.copy(amount = move.amount - 1))
    }
  }

  def crateMover9001(stacks: List[Stack], move: Move): List[Stack] = {
    val (upper, lower) = stacks(move.from).splitAt(move.amount)
    stacks.updated(move.to, upper ++ stacks(move.to))
      .updated(move.from, lower)
  }

  @tailrec
  def rotate(rows: List[List[Char]], rotated: List[Stack] = Nil): List[Stack] = {
    if (rows.isEmpty) {
      rotated
    } else {
      val topRow = rows.head
      val newRotated = topRow.zipAll(rotated, Nil, Nil).map {
        case (' ', list) => list
        case (c: Char, list) => c :: list
      }
      rotate(rows.tail, newRotated)
    }
  }

  @tailrec
  def parseMoves(input: List[String], moves: List[Move] = Nil): List[Move] = {
    if (input.isEmpty) {
      moves
    } else {
      parseMove(input.head) match {
        case Some(move) => parseMoves(input.tail, moves :+ move)
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
  def parseRows(input: List[String], stack: List[List[Char]] = Nil): List[List[Char]] = {
    val line = input.head
    if (bottomRegex.matcher(line).matches()) {
      stack
    } else {
      val row = for {
        i <- 1.to((line.length + 1) / 4)
      } yield line(4 * (i - 1) + 1)
      parseRows(input.tail, row.toList :: stack)
    }
  }

  assert(cratesOnTop.equals("VRWBSFZWM"))
  assert(cratesOnTop2.equals("RBTWJWMCF"))
}

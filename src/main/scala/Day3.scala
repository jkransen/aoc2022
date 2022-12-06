import scala.annotation.tailrec
import scala.io.Source

object Day3 extends App {

  val lowercaseOffset = 'a'.toInt - 1
  val uppercaseOffset = 'A'.toInt - 27

  def charToPriority(char: Char): Int = if (char.isUpper) {
    char.toInt - uppercaseOffset
  } else {
    char.toInt - lowercaseOffset
  }

  def countDoublesByPriority(firstHalf: String, secondHalf: String): Int = {
    val doubles = firstHalf.intersect(secondHalf).toSet
    doubles.foldLeft(0)((sum, next) => sum + charToPriority(next))
  }

  @tailrec
  def sumPriorities(input: List[String], sum: Int = 0): Int = {
    if (input.isEmpty) {
      sum
    } else {
      val nextLine = input.head
      val (firstHalf, secondHalf) = nextLine.splitAt(nextLine.length / 2)
      val doubles = countDoublesByPriority(firstHalf, secondHalf)
      sumPriorities(input.tail, sum + doubles)
    }
  }

  val lines: List[String] = Source.fromResource("day3_1").getLines().toList

  val total: Int = sumPriorities(lines)

  println(s"1. Sum of priorities: $total")

  def sharedPriority(groupOfElves: List[String]): Int = {
    val shared = groupOfElves.reduce((first, second) => first.intersect(second))
    charToPriority(shared.head)
  }

  val groups = lines.grouped(3).toList

  val sumSharedPriorities = groups.map(sharedPriority).sum

  println(s"2. Sum of shared priorities: $sumSharedPriorities")
}

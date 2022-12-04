import scala.annotation.tailrec
import scala.io.Source

object Day4 extends App {

  val lines: List[String] = Source.fromResource("day4_1").getLines().toList

  val regex = "(\\d+)-(\\d+),(\\d+)-(\\d+)".r

  def toRanges(line: String): (Range, Range) = {
    val matcher = regex.pattern.matcher(line)
    assert(matcher.matches(), s"Line does not match pattern: $line")
    (matcher.group(1).toInt
      .to(matcher.group(2).toInt),
    matcher.group(3).toInt
      .to(matcher.group(4).toInt))
  }

  @tailrec
  def countMatches(input: List[String], matches: Int, predicate: (Range, Range) => Boolean): Int = {
    if (input.isEmpty) {
      matches
    } else {
      val (first, second) = toRanges(input.head)
      if (predicate(second, first) || predicate(first, second)) {
        countMatches(input.tail, matches + 1, predicate)
      } else {
        countMatches(input.tail, matches, predicate)
      }
    }
  }

  def encompasses(first: Range, second: Range): Boolean = {
    first.start <= second.start && first.end >= second.end
  }

  println("1. Encompasses: " + countMatches(lines, 0, encompasses))

  def overlaps(first: Range, second: Range): Boolean = {
    first.intersect(second).nonEmpty
  }

  println("2. Overlaps: " + countMatches(lines, 0, overlaps))
}

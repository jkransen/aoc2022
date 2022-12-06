import scala.annotation.tailrec
import scala.io.Source

object Day6 extends App {

  val datastream: String = Source.fromResource("day6_1").mkString

  @tailrec
  def markerLocation(input: String, size: Int = 4, currentLocation: Int = 0): Int = {
    if (input.take(size).toSet.size == size) {
      currentLocation + size
    } else {
      markerLocation(input.tail, size, currentLocation + 1)
    }
  }

  assert(5 == markerLocation("bvwbjplbgvbhsrlpgdmjqwftvncz"))
  assert(6 == markerLocation("nppdvjthqldpwncqszvftbrmjlhg"))
  assert(10 == markerLocation("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"))
  assert(11 == markerLocation("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"))
  val solution1 = markerLocation(datastream)
  println(s"1. Marker location: $solution1")
  assert(1702 == solution1)

  def messageLocation(input: String): Int = {
    markerLocation(input, 14)
  }

  assert(19 == messageLocation("mjqjpqmgbljsphdztnvjfqwrcgsmlb"))
  val solution2 = messageLocation(datastream)
  println(s"2. Message location: $solution2")
  assert(3559 == solution2)
}

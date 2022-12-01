import java.lang.Integer.parseInt
import scala.annotation.tailrec
import scala.io.Source
import scala.util.Try

object Day1 extends App {

  val numbers: List[String] = {
    Source.fromResource("day1_1").getLines().toList
  }

  @tailrec
  def groupSum(input: List[String], currentElf: Int, aggregate: List[Int]): List[Int] = {
    if (input.isEmpty) {
      aggregate
    } else {
      val next = input.head
      Try(parseInt(next)).toOption match {
        case Some(value) => groupSum(input.tail, currentElf + value, aggregate)
        case None => groupSum(input.tail, 0, aggregate ++ List(currentElf))
      }
    }
  }

  println("Max: " + groupSum(numbers, 0, List()).max)

  private val top3: List[Int] = groupSum(numbers, 0, List()).sortWith((a, b) => a > b).take(3)
  println("Top 3: " + top3.sum)
}

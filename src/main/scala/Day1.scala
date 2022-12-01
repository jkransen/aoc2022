import java.lang.Integer.parseInt
import scala.annotation.tailrec
import scala.io.Source
import scala.util.Try

object Day1 extends App {

  val lines: List[String] = Source.fromResource("day1_1").getLines().toList

  @tailrec
  def groupSum(input: List[String], currentElf: Int, aggregate: List[Int]): List[Int] = {
    if (input.isEmpty) {
      aggregate
    } else {
      Try(parseInt(input.head)).toOption match {
        case Some(value) => groupSum(input.tail, currentElf + value, aggregate)
        case None => groupSum(input.tail, 0, aggregate ++ List(currentElf))
      }
    }
  }

  println("Max: " + groupSum(lines, 0, List()).max)

  val top3: List[Int] = groupSum(lines, 0, List()).sortWith((a, b) => a > b).take(3)
  println("Top 3: " + top3.sum)
}

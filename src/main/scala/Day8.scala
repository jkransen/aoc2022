import scala.annotation.tailrec
import scala.io.Source

object Day8 extends App {

  val lines: List[String] = Source.fromResource("day8_1").getLines().toList

  val matrix: List[List[Int]] = readMatrix(lines)
  val rows = matrix.size
  val cols = matrix.head.size

  val visibles: Seq[Boolean] = for {
    y <- 0 until rows
    x <- 0 until cols
  } yield isVisible(x, y)
  val visibleCount: Int = visibles.count(_ == true)
  println(s"1. Visible count: $visibleCount")

  val scenicScores: Seq[Int] = for {
    y <- 0 until rows
    x <- 0 until cols
  } yield (scenicScore(x, y))
  val maxScore: Int = scenicScores.max
  println(s"2. Max scenic score: $maxScore")

  def isVisible(x: Int, y: Int): Boolean = {
    val height = matrix(y)(x)
    val isVisibleLeft = 0.until(x).map(matrix(y)(_)).forall(_ < height)
    val isVisibleRight = (x + 1).until(cols).map(matrix(y)(_)).forall(_ < height)
    val isVisibleTop = 0.until(y).map(matrix(_)(x)).forall(_ < height)
    val isVisibleBottom = (y + 1).until(rows).map(matrix(_)(x)).forall(_ < height)
    isVisibleLeft || isVisibleRight || isVisibleTop || isVisibleBottom
  }

  def scenicScore(x: Int, y: Int): Int = {
    val height = matrix(y)(x)
    visibleLeft(x - 1, y, height) * visibleRight(x + 1, y, height) * visibleTop(x, y - 1, height) * visibleBottom(x, y + 1, height)
  }

  @tailrec
  def visibleLeft(x: Int, y: Int, maxHeight: Int, count: Int = 0): Int = {
    if (x < 0) {
      count
    } else if (matrix(y)(x) >= maxHeight) {
      count + 1
    } else {
      visibleLeft(x - 1, y, maxHeight, count + 1)
    }
  }

  @tailrec
  def visibleRight(x: Int, y: Int, maxHeight: Int, count: Int = 0): Int = {
    if (x >= cols) {
      count
    } else if (matrix(y)(x) >= maxHeight) {
      count + 1
    } else {
      visibleRight(x + 1, y, maxHeight, count + 1)
    }
  }

  @tailrec
  def visibleTop(x: Int, y: Int, maxHeight: Int, count: Int = 0): Int = {
    if (y < 0) {
      count
    } else if (matrix(y)(x) >= maxHeight) {
      count + 1
    } else {
      visibleTop(x, y - 1, maxHeight, count + 1)
    }
  }

  @tailrec
  def visibleBottom(x: Int, y: Int, maxHeight: Int, count: Int = 0): Int = {
    if (y >= rows) {
      count
    } else if (matrix(y)(x) >= maxHeight) {
      count + 1
    } else {
      visibleBottom(x, y + 1, maxHeight, count + 1)
    }
  }

  @tailrec
  def readMatrix(input: List[String], aggregate: List[List[Int]] = Nil): List[List[Int]] = {
    if (input.isEmpty) {
      aggregate
    } else {
      readMatrix(input.tail, aggregate :+ input.head.map(_.toInt - '0'.toInt).toList)
    }
  }
}

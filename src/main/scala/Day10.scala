import scala.annotation.tailrec
import scala.io.Source

object Day10 extends App {

  val noopRegex = "noop".r.regex
  val addxRegex = "addx (-?\\d+)".r.pattern

  val lines: List[String] = Source.fromResource("day10_2").getLines().toList
  val sampleInterval = 40
  val sampleClocks = 0.to(5).map(20 + _ * sampleInterval).toList
  println(sampleClocks.mkString(","))

  case class State(sampleClocks: List[Int], x: Int = 1, collectedSamples: List[Int] = List(), time: Int = 0)

  type Execution = State => State

  def abstractOperation(duration: Int, changeX: Int => Int): Execution = {
    state => {
      val currentClocks = 1.to(duration).map(state.time + _).toList
      currentClocks.foldLeft(state)( (state, clock) => {
        val (newCollectedSamples, remainingSampleClocks) = if (state.sampleClocks.contains(clock)) {
          val newSample = clock * state.x
          println(s"new sample $newSample at clock $clock and x ${state.x}")
          (state.collectedSamples :+ newSample, state.sampleClocks.filter(_ != clock))
        } else {
          (state.collectedSamples, state.sampleClocks)
        }
        val nextX = if (clock == currentClocks.max) changeX(state.x) else state.x
        State(remainingSampleClocks, nextX, newCollectedSamples, clock)
      })
    }
  }

  val noop = abstractOperation(1, x => x)

  def addX(deltaX: Int) = abstractOperation(2, x => x + deltaX)

  val operations = collectOperations(lines)

  val samples = collectSamples(operations, State(sampleClocks))
  samples.foreach(println)
  println(s"1. Sum of samples: ${samples.sum}")

  @tailrec
  def collectSamples(operations: List[Execution], previousState: State): List[Int] = {
    if (operations.isEmpty) {
      previousState.collectedSamples
    } else {
      val execution = operations.head
      val state = execution(previousState)
      if (state.sampleClocks.isEmpty) {
        state.collectedSamples
      } else {
        collectSamples(operations.tail, state)
      }
    }
  }

  @tailrec
  def collectOperations(input: List[String], aggregate: List[Execution] = Nil): List[Execution] = {
    if (input.isEmpty) {
      aggregate
    } else {
      val nextLine = input.head
      val addxMatcher = addxRegex.matcher(nextLine)
      if (noopRegex.matches(nextLine)) {
        collectOperations(input.tail, aggregate :+ noop)
      } else if (addxMatcher.matches()) {
        val deltax = addxMatcher.group(1).toInt
        collectOperations(input.tail, aggregate :+ addX(deltax))
      } else {
        collectOperations(input.tail, aggregate)
      }
    }
  }

  assert(13140 == samples.sum)
//  assert(12520 == samples.sum)
}

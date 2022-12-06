import scala.io.Source

object Day2 extends App {

  sealed trait HandShape {
    val defeats: HandShape
    val points: Int
  }
  object Rock extends HandShape {
    override val defeats: HandShape = Scissors
    override val points = 1
  }
  object Paper extends HandShape {
    override val defeats: HandShape = Rock
    override val points = 2
  }
  object Scissors extends HandShape {
    override val defeats: HandShape = Paper
    override val points = 3
  }

  case class Round(theirs: HandShape, ours: HandShape) {
    private val outcome = if (theirs.defeats == ours) {
      0
    } else if (ours.defeats == theirs) {
      6
    } else {
      3
    }
    val score: Int = ours.points + outcome
  }

  def charToTheirHand(char: Char): HandShape = char match {
    case 'A' => Rock
    case 'B' => Paper
    case 'C' => Scissors
  }

  def charToOurHand(char: Char): HandShape = char match {
    case 'X' => Rock
    case 'Y' => Paper
    case 'Z' => Scissors
  }

  def lineToRound(line: String): Round = {
    val theirs = charToTheirHand(line(0))
    val ours = charToOurHand(line(2))
    Round(theirs, ours)
  }

  val rounds: List[Round] = Source.fromResource("day2_1").getLines().map(lineToRound).toList

  val totalScore = rounds.map(_.score).sum

  println("1. Total score: " + totalScore)
  assert(totalScore == 14069)

  // can't iterate over sealed trait values :-(
  val handshapes = Set(Rock, Paper, Scissors)

  def lineToRound2(line: String): Round = {
    val theirs = charToTheirHand(line(0))
    val ours: HandShape = line(2) match {
      case 'X' => theirs.defeats
      case 'Y' => theirs
      case 'Z' => (handshapes - theirs - theirs.defeats).head
    }
    Round(theirs, ours)
  }

  val rounds2: List[Round] = Source.fromResource("day2_1").getLines().map(lineToRound2).toList

  val totalScore2 = rounds2.map(_.score).sum

  println("2. Total score: " + totalScore2)
  assert(totalScore2 == 12411)
}

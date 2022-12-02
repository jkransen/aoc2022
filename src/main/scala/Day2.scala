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

  sealed trait Whose
  case class Ours(hand: HandShape) extends Whose
  case class Theirs(hand: HandShape) extends Whose
  case object None extends Whose

  def charToHand(char: Char): Whose = {
    char match {
      case 'A' => Theirs(Rock)
      case 'B' => Theirs(Paper)
      case 'C' => Theirs(Scissors)
      case 'X' => Ours(Rock)
      case 'Y' => Ours(Paper)
      case 'Z' => Ours(Scissors)
      case _   => None
    }
  }

  case class Round(theirs: Theirs, ours: Ours) {
    val score: Int = {
      val outcome = if (theirs.hand.defeats == ours.hand) {
        0
      } else if (ours.hand.defeats == theirs.hand) {
        6
      } else {
        3
      }
      ours.hand.points + outcome
    }
  }

  def lineToRound(line: String): Round = {
    val theirs = line.map(charToHand)
      .filter(p => p.isInstanceOf[Theirs])
      .map(p => p.asInstanceOf[Theirs])
      .head
    val ours = line.map(charToHand)
      .filter(p => p.isInstanceOf[Ours])
      .map(p => p.asInstanceOf[Ours])
      .head
    Round(theirs, ours)
  }

  val rounds: List[Round] = Source.fromResource("day2_1").getLines().toList.map(lineToRound)

  val totalScore = rounds.map(_.score).sum

  println("1. Total score: " + totalScore)

  // can't iterate over sealed trait values :-(
  val handshapes = Set(Rock, Paper, Scissors)

  def lineToRound2(line: String): Round = {
    val theirs = charToHand(line.head).asInstanceOf[Theirs]
    val ourHand: HandShape = line(2) match {
      case 'X' => theirs.hand.defeats
      case 'Y' => theirs.hand
      case 'Z' => (handshapes - theirs.hand - theirs.hand.defeats).head
    }
    val ours = Ours(ourHand)
    Round(theirs, ours)
  }

  val rounds2: List[Round] = Source.fromResource("day2_1").getLines().toList.map(lineToRound2)

  val totalScore2 = rounds2.map(_.score).sum

  println("2. Total score: " + totalScore2)
}

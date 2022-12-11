import java.util.regex.Matcher
import scala.annotation.tailrec
import scala.io.Source

object Day11 extends App {

  val monkeyRegex = """Monkey (\d+):\s*Starting items: ([ ,\w]+)\s*Operation: new = ([ +*\w]+)\s*Test: divisible by (\d+)\s*If true: throw to monkey (\d+)\s*If false: throw to monkey (\d+)""".r.pattern

  val text = Source.fromResource("day11_1").mkString.replace('\n', ' ')
  println(text)

  case class Monkey(name: Int, items: List[Int], operation: Int => Int, divisibleBy: Int => Boolean, monkeyTrue: Int, monkeyFalse: Int, inspected: Int = 0)

  private val matcher = monkeyRegex.matcher(text)

  val monkeys = scanMonkeys(matcher)

  monkeys.foreach(println)

  val result = run(monkeys)

  println(result)

  @tailrec
  def run(monkeys: Map[Int, Monkey], remaining: Int = 20): Int = {
    if (remaining == 0) {
      monkeys.values.toList
        .map(_.inspected)
        .sortWith((one, two) => one > two)
        .take(2).product
    } else {
      println("round" + remaining)
      run(doRound(monkeys), remaining - 1)
    }
  }

  def doRound(monkeys: Map[Int, Monkey]): Map[Int, Monkey] = {
    val newMonkeys = monkeys.keys.toList.sorted.foldLeft(monkeys)((map, next) => doTurn(map, next))
    newMonkeys.foreach(println)
    newMonkeys
  }

  @tailrec
  def doTurn(monkeys: Map[Int, Monkey], active: Int): Map[Int, Monkey] = {
    val monkey = monkeys(active)
    if (monkey.items.isEmpty) {
      monkeys
    } else {
      val item = monkey.items.head
      val value = monkey.operation(item) / 3
      val target = if (monkey.divisibleBy(value)) {
        monkey.monkeyTrue
      } else {
        monkey.monkeyFalse
      }
      val targetMonkey = monkeys(target)
      val newTargetMonkey = targetMonkey.copy(items = targetMonkey.items :+ value)
      val newMonkey = monkey.copy(items = monkey.items.tail, inspected = monkey.inspected + 1)
      doTurn(monkeys.updated(active, newMonkey).updated(target, newTargetMonkey), active)
    }
  }

  @tailrec
  def scanMonkeys(matcher: Matcher, monkeys: Map[Int, Monkey] = Map()): Map[Int, Monkey] = {
    if (!matcher.find()) {
      monkeys
    } else {
      val name = matcher.group(1).toInt
      val items = matcher.group(2).split(", ").map(_.trim).map(_.toInt).toList
      val op = operation(matcher.group(3))_
      val divisible = divisibleBy(matcher.group(4).toInt)_
      val monkeyTrue = matcher.group(5).toInt
      val monkeyFalse = matcher.group(6).toInt
      val monkey = Monkey(name, items, op, divisible, monkeyTrue, monkeyFalse)
      scanMonkeys(matcher, monkeys + (monkey.name -> monkey))
    }
  }

  def operation(str: String)(old: Int): Int = {
    val replaced = str.replace("old", old.toString).split(" ").toList
    replaced match {
      case first :: "+" :: second :: rest => first.trim.toInt + second.trim.toInt
      case first :: "*" :: second :: rest => first.trim.toInt * second.trim.toInt
    }
  }

  def divisibleBy(by: Int)(in: Int): Boolean = {
    return in % by == 0
  }

  assert(81 == operation("old * old")(9))
  assert(18 == operation("old + old")(9))

  assert(divisibleBy(9)(81))

  // 56168 is too high
}

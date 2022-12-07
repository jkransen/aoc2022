import scala.annotation.tailrec
import scala.io.Source

object Day7 extends App {

  val root = "/"
  val cdRegex = """^\$ cd (.+)$""".r.pattern
  val fileRegex = """^(\d+) (.+)$""".r.pattern

  val lines: List[String] = Source.fromResource("day7_1").getLines().toList
  val unordered: Map[List[String], Int] = traverse(lines)

  val sizePerPath: Map[String, Int] = sumDirectorySizes(unordered)
  val smallDirs = sizePerPath.values.filter(_ < 100000)
  val answer1: Int = smallDirs.sum
  println(s"1. Sum of small dirs: ${answer1}")
  assert(1915606 == answer1)

  val total = 70000000
  val used = sizePerPath(root)
  val free = total - used
  val toBeFreed = 30000000 - free
  val bigEnoughDirs = sizePerPath.values.filter(_ > toBeFreed)
  val answer2: Int = bigEnoughDirs.min
  println(s"2. Smallest big enough dir: $answer2")
  assert(5025657 == answer2)

  @tailrec
  def traverse(input: List[String], filesByFullPath: Map[List[String], Int] = Map(), currentPath: List[String] = List()): Map[List[String], Int] = {
    if (input.isEmpty) {
      filesByFullPath
    } else {
      val line = input.head
      val cdMatcher = cdRegex.matcher(line)
      val fileMatcher = fileRegex.matcher(line)
      if (cdMatcher.matches()) {
        val path = cdMatcher.group(1)
        path match {
          case "/" => traverse(input.tail, filesByFullPath, List())
          case ".." => traverse(input.tail, filesByFullPath, currentPath.tail)
          case newDir => traverse(input.tail, filesByFullPath, newDir :: currentPath)
        }
      } else if (fileMatcher.matches()) {
        val fileName = fileMatcher.group(2)
        val fileSize = fileMatcher.group(1).toInt
        val fullPath = fileName :: currentPath
        traverse(input.tail, filesByFullPath + (fullPath -> fileSize), currentPath)
      } else {
        traverse(input.tail, filesByFullPath, currentPath)
      }
    }
  }

  @tailrec
  def sumDirectorySizes(unordered: Map[List[String], Int], sums: Map[String, Int] = Map()): Map[String, Int] = {
    if (unordered.isEmpty) {
      sums
    } else {
      val (path, fileSize) = unordered.head
      path match {
        case filename :: directories => {
          val newSums = addFileSizeToDirs(directories, sums, fileSize)
          sumDirectorySizes(unordered.tail, newSums)
        }
      }
    }
  }

  @tailrec
  def addFileSizeToDirs(directories: List[String], sums: Map[String, Int], fileSize: Int): Map[String, Int] = {
    if (directories.isEmpty) {
      sums + (root -> (sums.getOrElse(root, 0) + fileSize))
    } else {
      val fullPath = root + directories.reverse.mkString("/")
      val newSum = sums.getOrElse(fullPath, 0) + fileSize
      addFileSizeToDirs(directories.tail, sums + (fullPath -> newSum), fileSize)
    }
  }
}

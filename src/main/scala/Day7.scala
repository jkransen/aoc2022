import scala.annotation.tailrec
import scala.io.Source

object Day7 extends App {

  val root = "/"
  val cdRegex = """^\$ cd (.+)$""".r.pattern
  val fileRegex = """^(\d+) (.+)$""".r.pattern

  val lines: List[String] = Source.fromResource("day7_1").getLines().toList

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
        println(s"Change path to $path, current path: $currentPath")
        path match {
          case "/" => traverse(input.tail, filesByFullPath, List())
          case ".." => traverse(input.tail, filesByFullPath, currentPath.tail)
          case newDir => traverse(input.tail, filesByFullPath, newDir :: currentPath)
        }
      } else if (fileMatcher.matches()) {
        val fileName = fileMatcher.group(2)
        val fileSize = fileMatcher.group(1).toInt
        val fullPath = fileName :: currentPath
        println(s"Add file $fileName with size $fileSize to path /${currentPath.reverse.mkString("/")}")
        traverse(input.tail, filesByFullPath + (fullPath -> fileSize), currentPath)
      } else {
        traverse(input.tail, filesByFullPath, currentPath)
      }
    }
  }

  val unordered: Map[List[String], Int] = traverse(lines)
  println(unordered)

  @tailrec
  def sumSizes(unordered: Map[List[String], Int], sums: Map[String, Int] = Map()): Map[String, Int] = {
    if (unordered.isEmpty) {
      sums
    } else {
      val (path, fileSize) = unordered.head
      path match {
        case filename :: directories => {
          val newSums = addFileSizeToDirs(directories, sums, fileSize)
          sumSizes(unordered.tail, newSums)
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

  val sizePerPath = sumSizes(unordered)
  val smallDirs: Map[String, Int] = sizePerPath.filter(_._2 < 100000)
  println(smallDirs)
  val answer1: Int = smallDirs.values.sum
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
}

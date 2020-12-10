package day9

import java.util.Scanner
import java.io.File

import scala.collection.mutable.ListBuffer

class XmasChecker(filepath: String) {
  val scanner: Scanner = new Scanner(new File(filepath)).useDelimiter("\n")
  val numbers: List[BigInt] = scanInput()

  private def scanInput(): List[BigInt] = {
    val list: ListBuffer[BigInt] = ListBuffer()
    while(scanner.hasNext) { list += BigInt(scanner.next.toLong) }
    //println(list.mkString("\n"))
    list.toList
  }

  def verifyNumber(preamble: Set[BigInt], next: BigInt): Boolean = if(preamble.subsets(2).exists(_.sum == next)) true else false

  def findFirstInvalid(): BigInt = {
    val it = for {i <- 25 until numbers.length} yield numbers.slice(i - 25, i+1)
    for(slice <- it) {
      if(!verifyNumber(slice.init.toSet, slice.last)) return slice.last
    }
    -1
  }

  def lookIdk(num: BigInt): BigInt = {
    for(i <- numbers.indices) {
      var break = false
      var j = 2
      while(!break) {
        val slice = numbers.slice(i, i+j)
        if(slice.sum == num) return slice.min + slice.max
        else if(slice.sum < num) j += 1
        else if(slice.sum > num) break = true
      }
    }
    -1
  }

  def solvePart1(): Unit = println(findFirstInvalid())
  def solvePart2(): Unit = println(lookIdk(findFirstInvalid()))
}

package day5

import java.util.Scanner
import java.io.File

class SeatLocator(val filepath: String) {
  val scanner: Scanner = new Scanner(new File(filepath)).useDelimiter("\n")
  val passes: List[String] = scanInput()

  private def scanInput(): List[String] = {
    var list: List[String] = List()
    while(scanner.hasNext) {list :+= scanner.next}
    list
  }

  def findPositionByBinaryPartition(letters: String, initRange: Range): Int = {
    var reg: List[Int] = initRange.toList
    for(letter <- letters) {
      if("FL".contains(letter)) { reg = (reg.head to reg.last - (reg.last-reg.head)/2 + (-1)).toList }
      else if("BR".contains(letter)) { reg = (reg.head + (reg.last-reg.head)/2 + 1 to reg.last).toList }
    }
    reg.head
  }

  def findSeatId(pass: String): Int = {
    val rowLetters: String = pass take 7
    val columnLetters: String = pass drop 7
    val row: Int = findPositionByBinaryPartition(rowLetters, 0 to 127)
    val column: Int = findPositionByBinaryPartition(columnLetters, 0 to 7)
    println(s"Seat at column $column, row $row")
    8*row + column
  }

  def solvePart1(): Unit = println(passes.map(findSeatId).max)
  def solvePart2(): Unit = {
    val ids: List[Int] = passes.map(findSeatId).sorted
    for((first, second) <- ids.init zip ids.tail) {
      if(second - first == 2) println(first+1)
    }
  }
}

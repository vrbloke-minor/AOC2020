package day10

import java.util.Scanner
import java.io.File

import scala.collection.mutable.ListBuffer

class AdapterChecker(filepath: String) {
  val scanner: Scanner = new Scanner(new File(filepath)).useDelimiter("\n")
  val adapters: List[BigInt] = scanInput()

  private def scanInput(): List[BigInt] = {
    val list: ListBuffer[BigInt] = ListBuffer()
    while(scanner.hasNext) {list += BigInt(scanner.next.toInt)}
    println(list.mkString("\n"))
    list.sorted.toList
  }

  def solvePart1(): Unit = {}
  def solvePart2(): Unit = {}
}

package day10

import java.util.Scanner
import java.io.File

import scala.collection.mutable.ListBuffer

class AdapterChecker(filepath: String) {
  val scanner: Scanner = new Scanner(new File(filepath)).useDelimiter("\n")
  val adapters: List[Int] = scanInput()
  val chunks: List[List[Int]] = splitInSlices()

  private def scanInput(): List[Int] = {
    val list: ListBuffer[Int] = ListBuffer()
    while(scanner.hasNext) {list += scanner.next.toInt}
    //println(list.sorted.mkString("\n"))
    list.sorted.toList
  }

  def countDifferencesOfSize(diff: Int): Int = {
    (adapters.init zip adapters.tail).map(x => x._2 - x._1).count(_ == diff)
  }

  def solvePart1(): Unit = println((adapters.head + countDifferencesOfSize(1)) * (countDifferencesOfSize(3) + 1))

  def splitInSlices(): List[List[Int]] = {
    var bigList: ListBuffer[List[Int]] = ListBuffer()
    var smallList: List[Int] = List()
    for(i <- adapters.indices.tail) {
      if(adapters(i) - adapters(i-1) < 3) smallList :+= adapters(i-1)
      else {
        if(i == adapters.length - 1) smallList :+= adapters(i)
        if(smallList.nonEmpty) smallList = smallList.tail
        bigList += smallList
        smallList = List()
      }
    }
    bigList.filter(_.nonEmpty).tail.prepend(List(1,2,3)).append(List(167,168)).toList
  }
  def validPerms(chunk: List[Int]): BigInt = {
    chunk.length match {
      case 1 => 2
      case 2 => 4
      case 3 => 7
    }
  }
  def solvePart2(): Unit = println(chunks.map(validPerms).product[BigInt])
}

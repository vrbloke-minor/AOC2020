package day6

import java.util.Scanner
import java.io.File

class CustomsFormExaminer(filepath: String) {
  val scanner: Scanner = new Scanner(new File(filepath)).useDelimiter("\n\n")
  val groups: List[String] = scanInput()

  private def scanInput(): List[String] = {
    var list: List[String] = List()
    while(scanner.hasNext) {list :+= scanner.next()}
    //println(list.mkString(","))
    list
  }

  def countPresences(xs: String): Int = {
    var setOfPresent: Set[Char] = Set()
    for(ch <- xs) {
      if(ch != '\n') setOfPresent += ch
    }
    setOfPresent.size
  }

  def countOmnipresences(xs: String): Int = {
    val lines = xs.split('\n')
    val lineSets: Array[Set[Char]] = for {x <- lines} yield Set[Char]()
    for(lineIndex <- lineSets.indices) {
      for(ch <- lines(lineIndex)) {
        lineSets(lineIndex) += ch
      }
    }
    lineSets.reduce(_.intersect(_)).size
  }

  def solvePart1(): Unit = println(groups.map(countPresences).sum)
  def solvePart2(): Unit = println(groups.map(countOmnipresences).sum)
}

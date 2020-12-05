package day3

import java.util.Scanner
import java.io.File

class TobogganPathChecker(val filepath: String) {
  val scanner: java.util.Scanner = new Scanner(new File(filepath)).useDelimiter("\n")
  val map: List[String] = scanInput()

  private def scanInput(): List[String] = {
    var map: List[String] = List()
    while(scanner.hasNext) {map :+= scanner.next()}
    println(map)
    map
  }

  def countLooping(step: (Int,Int)): Long = {
    val xDim = map(1).length
    val yDim = map.length
    var (x,y) = (0,0)
    var count: Long = 0
    while(y < yDim) {
      if(map(y)(x) == '#') count += 1
      x = (x + step._1) % xDim
      y += step._2
    }
    count
  }

  def solvePart1(): Unit = println(countLooping((3,1)))
  def solvePart2(): Unit = println(List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2)).map(countLooping).product)
}

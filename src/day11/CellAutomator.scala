package day11

import scala.collection.mutable.ArrayBuffer
import scala.ref.SoftReference

import java.io.File
import java.util.Scanner

class CellAutomator(filepath: String) {
  val scanner: Scanner = new Scanner(new File(filepath)).useDelimiter("\n")
  val scanner2: Scanner = new Scanner(new File(filepath)).useDelimiter("\n")
  val (seatMap, xDim, yDim) = scanInput(scanner)
  val resetMap: Array[Array[Char]] = scanInput(scanner2)._1

  private def scanInput(scanner: Scanner): (Array[Array[Char]], Int, Int) = {
    val list: ArrayBuffer[Array[Char]] = ArrayBuffer()
    while(scanner.hasNext) { list += scanner.next.toCharArray }
    //println(list.map(_.mkString("")).mkString("\n"))
    val xDim = list.head.length
    val yDim = list.length
    (list.toArray, xDim, yDim)
  }

  def reset(): Unit = for(i <- 0 until xDim; j <- 0 until yDim) seatMap(j)(i) = resetMap(j)(i)

  def cushSlice(array: Array[Char], start: Int, until: Int): Array[Char] = {
    val toRet: ArrayBuffer[Char] = ArrayBuffer()
    for(i <- start until until) {
      try { toRet += array(i) }
      catch {case _:IndexOutOfBoundsException => '.'}
    }
    toRet.toArray
  }

  def cushApply(array: Array[Char], i: Int): Char = try {array(i)} catch {case _:IndexOutOfBoundsException => '.'}
  def cushApply(array: Array[Array[Char]], i: Int): Array[Char] = try {array(i)} catch {case _:IndexOutOfBoundsException => Array()}
  def getChar(x: Int, y: Int): Char = cushApply(cushApply(seatMap, y), x)

  def updateCell(x: Int, y: Int): (Char, Boolean) = seatMap(y)(x) match {
    case '.' => ('.', false)
    case 'L'|'#' =>
      val upSlice = cushSlice(cushApply(seatMap, y-1), x-1, x+2)
      val downSlice = cushSlice(cushApply(seatMap, y+1), x-1, x+2)
      val mdSlice = Array(cushApply(seatMap(y), x-1), cushApply(seatMap(y), x+1))
      val surroundings = upSlice ++ downSlice ++ mdSlice
      val occupied = surroundings.count{case '#' => true case _ => false}
     /* val w :Char = getChar(-1, 0)
      val nw:Char = getChar(-1,-1)
      val n :Char = getChar( 0,-1)
      val ne:Char = getChar( 1, 0)
      val se:Char = getChar( 1, 1)
      val s: Char = getChar( 0, 1)
      val sw:Char = getChar(-1, 1)
      val occupied = Array(w, nw, n, ne, se, s, sw).count{_ == '#'}*/
      seatMap(y)(x) match {
        case 'L' => if(occupied == 0) ('#', true) else ('L', false)
        case '#' => if(occupied >= 4) ('L', true) else ('#', false)
      }
  }

  def updateCell2(x: Int, y: Int): (Char, Boolean) = seatMap(y)(x) match {
    case '.' => ('.', false)
    case '#'|'L' =>
      val w: Char = findSeat(x,y,-1, 0)
      val nw:Char = findSeat(x,y,-1,-1)
      val n: Char = findSeat(x,y, 0,-1)
      val ne:Char = findSeat(x,y, 1,-1)
      val e: Char = findSeat(x,y, 1, 0)
      val se:Char = findSeat(x,y, 1, 1)
      val s: Char = findSeat(x,y, 0, 1)
      val sw:Char = findSeat(x,y,-1, 1)
      val occupied = Array(w, nw, n, ne, e, se, s, sw).count{_ == '#'}
      seatMap(y)(x) match {
        case 'L' => if(occupied == 0) ('#', true) else ('L', false)
        case '#' => if(occupied >= 5) ('L', true) else ('#', false)
      }
  }

  def findSeat(x: Int, y: Int, dx: Int, dy: Int): Char = {
    var seat: Char = '.'
    var currX = x + dx
    var currY = y + dy
    var break: Boolean = false
    while(seat == '.' && !break) {
      try {
        seat = seatMap(currY)(currX)
        currX += dx
        currY += dy
      } catch { case _:IndexOutOfBoundsException => seat = '.'; break = true }
    }
    seat
  }

  def simulateStep(upd: (Int,Int) => (Char, Boolean)): Boolean = {
    val newBoard: Array[Array[Char]] = Array.ofDim[Char](yDim, xDim)
    var anyChanged: Boolean = false
    for(i <- 0 until xDim; j <- 0 until yDim) {
      val (newCell, changed) = upd(i, j)
      newBoard(j)(i) = newCell
      anyChanged = if(anyChanged) anyChanged else changed
    }
    for(i <- 0 until xDim; j <- 0 until yDim) seatMap(j)(i) = newBoard(j)(i)
    anyChanged
  }

  def simulateUntilNoChanges(upd: (Int,Int) => (Char, Boolean)): Unit = {
    while(simulateStep(upd)) {}
    println(seatMap.map(_.count{_ == '#'}).sum)
  }

  def solvePart1(): Unit = {simulateUntilNoChanges(updateCell); println(seatMap sameElements resetMap);reset()}
  def solvePart2(): Unit = {simulateUntilNoChanges(updateCell2); reset()}
}

package day12

import java.util.Scanner
import java.io.File

import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

class BoatController(filepath: String) {
  val scanner: Scanner = new Scanner(new File(filepath)).useDelimiter("\n")
  val regex: Regex = raw"([A-Z])([0-9]+)".r
  val commands: List[(Char, Int)] = scanInput()
  var direction: Int = 0
  var posX: Int = 0
  var posY: Int = 0
  var dX: Int = 10
  var dY: Int = 1

  private def scanInput(): List[(Char, Int)] = {
    val list: ListBuffer[(Char,Int)] = ListBuffer()
    while(scanner.hasNext) {
      val m = regex.findFirstMatchIn(scanner.next).getOrElse{throw new Exception; null}.subgroups
      list += Tuple2(m.head.head, m.last.toInt)
    }
    //println(list.filter(x => x._1 == 'L' ||  x._2 == 'R').mkString("\n"))
    list.toList
  }

  def executeMisunderstood(cmd: (Char, Int)): Unit = cmd._1 match {
    case 'N' => posY += cmd._2
    case 'S' => posY -= cmd._2
    case 'E' => posX += cmd._2
    case 'W' => posX -= cmd._2
    case 'L' => direction = forceDeg(direction + cmd._2)
    case 'R' => direction = forceDeg(direction - cmd._2)
    case 'F' => direction match {
      case 0   => posX += cmd._2
      case 90  => posY += cmd._2
      case 180 => posX -= cmd._2
      case 270 => posY -= cmd._2
    }
  }

  def executeCorrect(cmd: (Char,Int)): Unit = cmd._1 match {
    case 'N' => dY += cmd._2
    case 'S' => dY -= cmd._2
    case 'E' => dX += cmd._2
    case 'W' => dX -= cmd._2
    case 'L'|'R' => val (tX, tY) = (dX, dY); (if(cmd._1 == 'R') forceDeg(-cmd._2) else cmd._2) match {
      case 0   =>
      case 90  => dX = -tY; dY = tX;
      case 180 => dX = -tX; dY = -tY
      case 270 => dX = tY; dY = -tX
    }
    case 'F' =>
      posX += (cmd._2 * dX)
      posY += (cmd._2 * dY)
  }

  def getDistance: Int = posX.abs + posY.abs
  def forceDeg(arg: Int): Int = {
    if(arg >= 360) arg % 360
    else if(arg < 0) ((if(arg.abs/360==0) 1 else arg.abs/360)*360 + arg).abs
    else arg
  }

  def setSail(executeCommand: ((Char, Int)) => Unit): Int = {commands.foreach(executeCommand); getDistance}

  def solvePart1(): Unit = {println(setSail(executeMisunderstood)); posX = 0; posY = 0}
  def solvePart2(): Unit = {println(setSail(executeCorrect)); posX = 0; posY = 0}
}

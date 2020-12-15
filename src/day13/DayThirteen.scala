package day13

import java.util.Scanner
import java.io.File

import scala.collection.mutable.ArrayBuffer

class DayThirteen(filepath: String) {
  val scanner: Scanner = new Scanner(new File(filepath)).useDelimiter("\n")
  val (tmstamp, busIDs, modulos) = scanInput()

  private def scanInput(): (Int, List[BigInt], List[BigInt]) = {
    val tmstamp = scanner.next.toInt
    val line2 = scanner.next
    val busIDs = line2.split(",").filterNot(_ == "x").map(x => BigInt(x.toInt)).toList
    val offsets = busIDs.map(x => (x - (line2.split(",").map(_.toIntOption).indexWhere(_.getOrElse(0) == x) % x))%x)
    println(offsets)
    (tmstamp, busIDs, offsets)
  }

  def findSoonest(): BigInt = {
    val stampModBus: List[BigInt] = busIDs.map(tmstamp % _)
    val soonest = (stampModBus zip busIDs).map(tp => (tp._2 - tp._1, tp._2)).minBy(_._1)
    soonest._2 * soonest._1
  }

  def solveCRT(rem: List[BigInt], num: List[BigInt]): BigInt = {
    val x: Array[BigInt] = Array.ofDim[BigInt](busIDs.length).map(x => BigInt(-1))
    x(0) = rem.head
    for(i <- 1 until x.length) {
      val prod: BigInt = num.slice(0, i).product
      var xi = x(i-1)
      while(x(i) == -1) {
        xi += prod
        if(xi % num(i) == rem(i)) x(i) = xi
      }
    }
    x.last
  }

  def solvePart1(): Unit = println(findSoonest())
  def solvePart2(): Unit = println(solveCRT(modulos, busIDs))
}

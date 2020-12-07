package day7

import java.util.Scanner
import java.io.File

import scala.annotation.tailrec
import scala.util.matching.Regex

class BagChecker(val filepath: String) {
  class Bag(val name: String, val list: List[String]) {
    override def toString: String = s"BAG NAMED $name, CONTAINS ${list.mkString("(",",",")")}"
  }
  val scanner: Scanner = new Scanner(new File(filepath)).useDelimiter("\n")
  val bagKinds: List[Bag] = scanInput()
  val containRegex: Regex = raw" ([0-9]+) (.+) bags?".r
  val emptyRegex: Regex = raw" no other bags.".r
  val startRegex: Regex = raw"(.+) bags contain".r

  private def scanInput(): List[Bag] = {
    @tailrec
    def addManyTimes[T](arg: T, times: Int, list: List[T] = List()): List[T] = {
      if(times == 0) list
      else addManyTimes(arg, times - 1, list :+ arg)
    }

    var list = List[Bag]()
    while(scanner.hasNext) {
      val line = scanner.next
      val nameOfBag = startRegex.findFirstMatchIn(line).get.group(1)
      val contBagsRaw: List[List[String]] = if(line.split("\n").length == 1) List() else
        line.split("\n").map(containRegex.findFirstMatchIn(_).get.subgroups).toList
      var contBags = List[String]()
      contBagsRaw.foreach((xs: List[String]) => contBags ++= addManyTimes(xs.last.toInt, xs.head.toInt))
      list :+= new Bag(nameOfBag, contBags)
    }
    println(list.mkString("\n"))
    list
  }

  def solvePart1(): Unit = {}
  def solvePart2(): Unit = {}
}

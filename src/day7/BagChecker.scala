package day7

import java.util.Scanner
import java.io.File

import scala.annotation.tailrec
import scala.util.matching.Regex

class BagChecker(val filepath: String) {
  class Bag(val name: String, val list: List[String]) {
    var listBags: List[Bag] = List()
    var listBagsNoRepeats: List[Bag] = List()
    override def toString: String = s"$name"
  }
  val scanner: Scanner = new Scanner(new File(filepath)).useDelimiter("\n")
  val containRegex: Regex = raw" ([0-9]+) (.+) bags?".r
  val emptyRegex: Regex = raw" no other bags.".r
  val startRegex: Regex = raw"(.+) bags contain".r
  val bagKinds: List[Bag] = improvedBagList()

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
      val contBagsRaw: List[List[String]] = if(emptyRegex.findFirstMatchIn(line).nonEmpty) List() else
        if(line.split(",").length == 1) List(containRegex.findFirstMatchIn(line).get.subgroups) else
          line.split(",").map(containRegex.findFirstMatchIn(_).get.subgroups).toList
      var contBags = List[String]()
      contBagsRaw.foreach((xs: List[String]) => contBags ++= addManyTimes(xs.last, xs.head.toInt))
      list :+= new Bag(nameOfBag, contBags)
    }
    list
  }

  private def improvedBagList(): List[Bag] = {
    val list = scanInput()
    for(outerBag <- list) {
      for(innerBag <- outerBag.list) {
        val foundBag = list.find(_.name == innerBag).get
        outerBag.listBags :+= foundBag
      }
      outerBag.listBagsNoRepeats = outerBag.listBags.distinct
    }
    list
  }

  def bagContains(start: Bag, x: String): Boolean = {
    var stack = List(start)
    while(stack.nonEmpty) {
      val current = stack.head
      stack = stack.tail

      if(current.list.contains(x)) { println(s"Returning true! Found in bag $current"); return true }

      stack ++= current.listBagsNoRepeats
    }
    false
  }

  def bagsInBag(startName: String): Long = {
    val start: Bag = bagKinds.find(_.name == startName).getOrElse{println("OH MY GOD NO"); null}
    if(start == null) -10
    var stack = List(start)
    var result: Long = 0
    while(stack.nonEmpty) {
      val current = stack.head
      stack = stack.tail

      result += current.listBags.length

      stack ++= current.listBags
    }
    result
  }

  def solvePart1(): Unit = println(bagKinds.map(x => if (bagContains(x, "shiny gold")) 1 else 0).sum)
  def solvePart2(): Unit = println(bagsInBag("shiny gold"))
}

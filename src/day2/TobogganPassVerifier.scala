package day2

import java.io.File
import java.util.Scanner

class TobogganPassVerifier(filepath: String) {
  val scanner: java.util.Scanner = new Scanner(new File(filepath)).useDelimiter("\n")
  val annotatedPasswords: List[(Int, Int, Char, String)] = scanAnnotatedPasswords()

  def scanAnnotatedPasswords(): List[(Int, Int, Char, String)] = {
    var annotatedPasswords: List[(Int, Int, Char, String)] = List()
    while(scanner.hasNext) {
      val next = scanner.next().split(" ")
      annotatedPasswords :+= Tuple4(next(0).split("-")(0).toInt, next(0).split("-")(1).toInt,
                            next(1)(0), next(2))
    }
    //println(annotatedPasswords.mkString("\n"))
    annotatedPasswords
  }

  def countValidPasswords(rule: ((Int, Int, Char, String)) => Boolean): Int = {
    annotatedPasswords.map(rule).groupBy(identity).getOrElse(true, List()).length
  }

  def appearancesInRange(annotatedPassword: (Int,Int,Char,String)): Boolean = {
    val (lowBound, highBound, character, password) = annotatedPassword
    (lowBound to highBound).contains(password.map(_==character).groupBy(identity).getOrElse(true, List()).length)
  }

  def exactPositionsMatch(annotatedPassword: (Int,Int,Char,String)): Boolean = {
    val (firstPos, secondPos, character, password) = annotatedPassword
    (password(firstPos-1) == character) != (password(secondPos-1) == character)
  }

  def solvePart1(): Unit = println(countValidPasswords(appearancesInRange))
  def solvePart2(): Unit = println(countValidPasswords(exactPositionsMatch))
}

package day2

import java.io.File
import java.util.Scanner

class TobogganPassVerifier(filepath: String) {
  val scanner = new Scanner(new File(filepath))
  val annotatedPasswords: List[(Int, Int, Char, String)] = scanAnnotatedPasswords()

  def scanAnnotatedPasswords(): List[(Int, Int, Char, String)] = {
    var annotatedPasswords: List[(Int, Int, Char, String)] = List()
    while(scanner.useDelimiter("\n").hasNext) {
      val next = scanner.next().split(" ")
      annotatedPasswords :+= (next(0).split("-")(0), next(0).split("-")(1),
                            next(1)(0), next(2))
    }
    println(annotatedPasswords.mkString("\n"))
    annotatedPasswords
  }
}

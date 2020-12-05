package day4

import java.util.Scanner
import java.io.File

import scala.util.matching.Regex

class IdChecker(val filepath: String) {
  val scanner: Scanner = new Scanner(new File(filepath)).useDelimiter("\n\n")
  val ids: List[List[String]] = scanInput()
  val requiredFields = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
  val hgtRegex: Regex = raw"([0-9]+)(cm|in)".r
  val hclRegex: Regex = raw"(#)([0-9a-f]{6})".r
  val eclRegex: Regex = raw"(amb|blu|brn|gry|grn|hzl|oth)".r
  val pidRegex: Regex = raw"([0-9]{9})".r

  private def scanInput(): List[List[String]] = {
    var list: List[List[String]] = List()
    while(scanner.hasNext) {list :+= scanner.next.replace('\n', ' ').split(" ").toList}
    list
  }

  def verifyPresence(id: List[String]): Boolean = {
    id.map((field: String) => if(requiredFields.contains(field.split(":").head)) 1 else 0).sum == requiredFields.size
  }

  def verifyData(id: List[String]): Boolean = {
    var validFields: Int = 0
    for(fieldString <- id) {
      val fieldSplit = fieldString.split(":")
      val (field, data) = (fieldSplit.head, fieldSplit.last)
      field match {
        case "byr" =>
          validFields += (if((1920 to 2002).contains(data.toInt)) 1 else 0)
        case "iyr" =>
          validFields += (if((2010 to 2020).contains(data.toInt)) 1 else 0)
        case "eyr" =>
          validFields += (if((2020 to 2030).contains(data.toInt)) 1 else 0)
        case "hgt" =>
          data match {
            case hgtRegex(num, "cm") =>
              validFields += (if((150 to 193).contains(num.toInt)) 1 else 0)
            case hgtRegex(num, "in") =>
              validFields += (if((59 to 76).contains(num.toInt)) 1 else 0)
            case _ =>
              validFields += 0
          }
        case "hcl" =>
          validFields += (if(hclRegex.matches(data)) 1 else 0)
        case "ecl" =>
          validFields += (if(eclRegex.matches(data)) 1 else 0)
        case "pid" =>
          validFields += (if(pidRegex.matches(data)) 1 else 0)
        case _ =>
          validFields += 0
      }
    }
    validFields == requiredFields.size
  }

  def countValidIds(rule: List[String] => Boolean): Int = ids.map((id: List[String]) => if(rule(id)) 1 else 0).sum
  def solvePart1(): Unit = println(countValidIds(verifyPresence))
  def solvePart2(): Unit = println(countValidIds(verifyData))
}

package day8

import java.util.Scanner
import java.io.File

import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

class AssemblyComputer(val filepath: String) {
  class Op(var arg: Int, var repeat: Int = 0)
  case class Acc(a: Int) extends Op(a) { override def toString: String = s"ACC $arg, repeat $repeat"}
  case class Jmp(a: Int) extends Op(a) { override def toString: String = s"JMP $arg, repeat $repeat"}
  case class Nop(a: Int) extends Op(a) { override def toString: String = s"NOP $arg, repeat $repeat"}

  val scanner: Scanner = new Scanner(new File(filepath)).useDelimiter("\n")
  val opRegex: Regex = raw"([a-z]{3}) (-?[0-9]+|\+?[0-9]+)".r
  val code: ArrayBuffer[Op] = scanInput()
  val accs: ArrayBuffer[Op] = code.collect{case x: Acc => x}
  val jmps: ArrayBuffer[Op] = code.collect{case x: Jmp => x}
  val nops: ArrayBuffer[Op] = code.collect{case x: Nop => x}
  var head: Int = 0
  var accumulator: Int = 0

  private def scanInput(): ArrayBuffer[Op] = {
    val list: ArrayBuffer[Op] = ArrayBuffer()
    while(scanner.hasNext) {
      val next = scanner.next
      //println(next)
      list.append( next match {
      case opRegex("acc", arg) => Acc(arg.toInt)
      case opRegex("jmp", arg) => Jmp(arg.toInt)
      case opRegex("nop", arg) => Nop(arg.toInt)
    })}
    list
  }

  private def clearRepeats(): Unit = {
    code.foreach(_.repeat = 0)
  }

  // Executes Op and returns Op next to execute, for possible inspection
  private def executeCurrentOp(): Option[Op] = {
    val curr: Op = code(head)
    curr match {
      case x: Acc =>
        accumulator += x.arg
        head += 1
      case x: Jmp =>
        head += x.arg
      case x: Nop =>
        head += 1
    }
    curr.repeat += 1
    try { Some(code(head)) } catch { case e: IndexOutOfBoundsException => None }
  }

  def executeUntilRepeat(): Unit = while(executeCurrentOp().getOrElse{println("Code does not loop as expected"); null}.repeat != 1) {}
  def executeUntilTermination(): Unit = while(executeCurrentOp().nonEmpty) {}
  def executeUntilTerminationOrLoop(): Unit = {
    var t: Boolean = true
    while(t) {
      val nextOp = executeCurrentOp()
      t = nextOp.nonEmpty && nextOp.getOrElse{println("This code should be unreachable"); null}.repeat != 1
    }
  }

  def findTerminating(): Option[Int] = {
    var lastIndex = 0
    println("Searching accs")
    while(code.indexWhere({case x: Acc => true; case _ => false}, lastIndex) != -1) {
      val i = code.indexWhere({case x: Acc => true; case _ => false}, lastIndex) + 1
      println(i)
      lastIndex = i+1
      val temp = code(i)
      code(i) = Nop(code(i).arg)
      executeUntilTerminationOrLoop()
      if(head >= code.length) return Some(accumulator)
      else {
        head = 0
        clearRepeats()
        accumulator = 0
        code(i) = temp
      }
    }
    lastIndex = 0
    println("Searching nops")
    while(code.indexWhere({case x: Nop => true; case _ => false}, lastIndex) != -1) {
      //println("New iter")
      val i = code.indexWhere({case x: Nop => true; case _ => false}, lastIndex) + 1
      println(i)
      lastIndex = i+1
      val temp = code(i)
      code(i) = Acc(code(i).arg)
      executeUntilTerminationOrLoop()
      if(head >= code.length) return Some(accumulator)
      else {
        head = 0
        clearRepeats()
        accumulator = 0
        code(i) = temp
      }
    }
    None
  }

  def solvePart1(): Unit = { executeUntilRepeat(); println(accumulator) }
  def solvePart2(): Unit = { println(findTerminating().getOrElse{println("MAJOR ISSUES"); null}) }
}

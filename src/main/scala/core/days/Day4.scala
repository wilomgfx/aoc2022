package core.days

import core.InputReader

object Day4 {
  type CleaningRange = (Int, Int)

  private def getRangeInString(string: String): CleaningRange = {
    val (rangeStart, rangeEnd) = string.split('-').splitAt(1)
    (rangeStart.head.toInt, rangeEnd.head.toInt)
  }

  private def getAssignments(string: String): (CleaningRange, CleaningRange) = {
    val (elf1, elf2) = string.split(',').splitAt(1)
    val elf1Range = getRangeInString(elf1.head)
    val elf2Range = getRangeInString(elf2.head)
    (elf1Range, elf2Range)
  }

  private def findOverlaps(string: String): Boolean = {
    val (
      startSide1: Boolean,
      endSide1: Boolean,
      startSide2: Boolean,
      endSide2: Boolean
    ) = getRangeContainment(string)
    (startSide1 && endSide1) || (startSide2 && endSide2)
  }

  def part1(): Int = {
    val input = InputReader.read("day4/part1.txt").split("\n")
//    println(input.mkString("\n"))
    val overLaps = input.map(i => findOverlaps(i))
//    println(overLaps.mkString("\n"))
    val overlapsCount = overLaps.count(_ == true)
//    assert(overlapsCount == 2)
    overlapsCount
  }

  private def findAnyOverlaps(string: String): Boolean = {
    val (
      startSide1: Boolean,
      endSide1: Boolean,
      startSide2: Boolean,
      endSide2: Boolean
    ) = getRangeContainment(string)
    (startSide1 || endSide1) || (startSide2 || endSide2)
  }

  private def getRangeContainment(string: String) = {
    val (elf1, elf2) = getAssignments(string)
    //    println(elf1, elf2)
    val startSide1 = (elf1._1 to elf1._2 contains elf2._1)
    val endSide1 = (elf1._1 to elf1._2 contains elf2._2)
    val startSide2 = (elf2._1 to elf2._2 contains elf1._1)
    val endSide2 = (elf2._1 to elf2._2 contains elf1._2)
    (startSide1, endSide1, startSide2, endSide2)
  }

  def part2(): Int = {
    val input = InputReader.read("day4/part1.txt").split("\n")
    //    println(input.mkString("\n"))
    val overLaps = input.map(i => findAnyOverlaps(i))
    //    println(overLaps.mkString("\n"))
    val overlapsCount = overLaps.count(_ == true)
//    assert(overlapsCount == 4)
    overlapsCount
  }

  def main(args: Array[String]) = {
    println("Part1")
    println(part1())
    println("Part2")
    println(part2())
  }

}

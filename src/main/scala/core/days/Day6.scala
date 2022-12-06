package core.days

import core.InputReader

import scala.util.control.Breaks.break

object Day6 {

  private def getMarkerIndex(
      input: Array[String],
      numOfChars: Int
  ): Array[Int] = {
    val marker = input.map(
      (
          i =>
            i.toCharArray
              .sliding(numOfChars, 1)
              .indexWhere(_.distinct.length == numOfChars) + numOfChars
      )
    )
    marker
  }

  def part1(): Int = {
    val input = InputReader.read("day6/input.txt").split("\n")
    val markerIndex = getMarkerIndex(input, 4)
    markerIndex.head
  }

  def part2(): Int = {
    val input = InputReader.read("day6/input.txt").split("\n")
    val markerIndex = getMarkerIndex(input, 14)
    markerIndex.head
  }

  def main(args: Array[String]): Unit = {
    println("Part1")
    println(part1())
    println("Part2")
    println(part2())
  }

}

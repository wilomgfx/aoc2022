package core.days

import core.InputReader

object Day1 {

  private def getSums: Array[Int] = {
    val input = InputReader.read("day1/part1.txt")

    val sums = input.split("\n\n").map(in => in.split("\n").foldLeft(0) {
      (sum, item) => {
        sum + item.toInt
      }
    })
    sums
  }
  def part1(): Int = {
    getSums.max
  }

  def part2(): Int = {
    getSums.sortWith(_ > _).take(3).sum
  }
}
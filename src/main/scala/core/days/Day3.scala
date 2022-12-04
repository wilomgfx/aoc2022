package core.days

import core.InputReader



object Day3 {

  type Compartment = (String, String)

  private def getCompartments(rucksack: String): Compartment = {
      val size = rucksack.length / 2
      rucksack.splitAt(size)
  }

  private def findDuplicatesInCompartments(compartment: Compartment): Char = {
    val (right, left) = compartment
    var duplicates = Seq[Char]()
    right.foreach(c => {
      if (left.contains(c))
        duplicates = duplicates :+ c
    })
    duplicates.distinct.head

  }

  private def getItemPriority(item: Char): Int = {
    var priority = -1
    if(item.isLower) {
      priority = item - 'a' + 1

    } else {
      priority = item.toLower - 'a' + 27
    }
    priority
  }

  def part1(): Int = {
    val input = InputReader.read("day3/part1.txt").split("\n")
//    input.foreach(println)
    val compartments = input.map(i => getCompartments(i))
    val duplicates = compartments.map(c => {
      findDuplicatesInCompartments(c)
    })
    val priorities = duplicates.map(d => getItemPriority(d))
//    println(priorities.mkString("\n"))
//    println(duplicates.mkString("\n"))
//     println(compartments.mkString("\n"))
    val sum = priorities.sum
//    assert(sum == 157)
    sum
  }

  private def findGroups(strings: Array[String]): Seq[Array[String]] = {
    strings.grouped(3).toSeq
  }

  private def findDuplicates(strings: Array[String]): Char = {
    var duplicateChars = Seq[Char]()
    val (first, rest) = strings.splitAt(1)
    val (second, third) = rest.splitAt(1)
    first.head.foreach(c => {
      if (second.head.contains(c) && third.head.contains(c)) {
        duplicateChars = duplicateChars :+ c
      }
    })

    duplicateChars.distinct.head
  }

  def part2(): Int = {
    val input = InputReader.read("day3/part1.txt").split("\n")
    val groups = findGroups(input)
    val duplicates = groups.map(g => findDuplicates(g))
//    println(duplicates.mkString("\n"))
    val priorities = duplicates.map(d => getItemPriority(d))
    val sum = priorities.sum
//        assert(sum == 70)
    sum

  }

  def main(args: Array[String]) = {
    println("Part1")
    println(part1())
    println("Part2")
    println(part2())
  }

}

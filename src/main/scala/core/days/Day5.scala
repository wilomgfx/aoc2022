package core.days

import core.InputReader

import scala.collection.mutable

object Day5 {

  private def getStacksContent(strings: Array[String]): Array[String] = {
    val indexRow = strings.length - 1
    val contentRows = strings.splitAt(indexRow)._1
    // TODO: investigate why contentRows.max does not work all the time
    val maxContentRowLength =
      contentRows(indexRow - 1).split(" ").count(_.nonEmpty)

    /* padding logic
    contentRows(0).replaceAll("    ", "-").padTo(5, "-").mkString.replaceAll("-", "[-]")
    [-][D][-]
     contentRows(0).replaceAll("    ", "-").padTo(newPadding + 3 * maxContentRowLength , "-").mkString.replaceAll("-", "[-]")
     */
    // TODO: fix parsing, I had to manually add the " - " in the input to make it work for the real input. Most likely not working because of the 4 spaces replace.
    // It only works for the `example.txt`
    val paddedContent = contentRows.map(s => {
//      val leftReplace = s.replaceAll("    ", "-")
      val padIndexStart = s.length + 1
      val replaceTo =
        if (padIndexStart < maxContentRowLength * 4) padIndexStart else 0
//      val leftPad = s.padTo(replaceTo, '-')
      val newRow = s.mkString.replaceAll("-", "[-]")
      newRow.replaceAll(" ", "")
    })

//    println(paddedContent.mkString("\n"))

    paddedContent

  }

  private def getStacksIndices(strings: Array[String]) = {
    val indexRow = strings(strings.length - 1)
    val indices =
      indexRow.strip().split(" ").filter(_.nonEmpty)
//    println(indices.mkString)
    indices.map(_.toInt)
  }
  private def createStacksMapping(
      strings: Array[String]
  ): Map[Int, mutable.Stack[String]] = {

//    val indexRow = strings.length - 1
//    val contentRows = strings.splitAt(indexRow)._1
//    // TODO: investigate why contentRows.max does not work all the time
//    val maxContentRowLength =
//      contentRows(indexRow - 1).split(" ").count(_.nonEmpty)

    var map = Map[Int, mutable.Stack[String]]()
    val content = getStacksContent(strings)
//    println(content.mkString("\n"))
    val indices = getStacksIndices(strings)
    indices.foreach(i => map = map + (i -> new mutable.Stack()))
    content.zipWithIndex.foreach {

      case (elem, idx) => {
        // keep delimiter (lookbehind)
        val rows = elem.split("(?<=])")
        rows.zipWithIndex.foreach {
          case (row, rIdx) => {
            val list = map(rIdx + 1)
            if (!row.equals("[-]"))
              list.append(row)
            map = map + (rIdx + 1 -> list)
          }
        }
      }

    }
    map
  }

  private def parseMove(string: String) = {
    // nasty, I should get better at regexes :crylaugh:
    val splits = string.split("[^0-9]").filter(_.nonEmpty)
    val move = splits(0)
    val from = splits(1)
    val to = splits(2)
    (move.toInt, from.toInt, to.toInt)
  }

  private def makeMoves(
      mapping: Map[Int, mutable.Stack[String]],
      moves: Array[String]
  ): Map[Int, mutable.Stack[String]] = {
    moves.foreach(m => {
      val (numberOfMoves, from, to) = parseMove(m)
//      println(numberOfMoves, from, to)

      for (i <- 0 until numberOfMoves) {
//        println(s"----${i}")
        val fromQueue = mapping(from)
        if (fromQueue.nonEmpty) {
          val itemToMove = fromQueue.pop()
          val toQueue = mapping(to)
          toQueue.prepend(itemToMove)
        }

//        println(mapping.mkString("\n"))
      }

    })
    mapping
  }

  def part1(): String = {
    val input = InputReader.read("day5/input.txt").split("\n\n")
    val (stacks, moves) = input.splitAt(1)
//     create FIFOS
    val mapping = createStacksMapping(stacks.head.split("\n"))
//    println(mapping.mkString("\n"))
    val mappingAfterMoves =
      makeMoves(mapping, moves.head.split("\n")).toSeq.sortBy(_._1)
//    println(mappingAfterMoves.mkString("\n"))
    val topCrates = mappingAfterMoves
      .map(m => m._2(0).replaceAll("\\[", "").replaceAll("\\]", ""))
      .mkString
    topCrates
//    println(topCrates)
//    assert(topCrates == "CMZ")

  }

  private def makeMovesCrateMover9000(
      mapping: Map[Int, mutable.Stack[String]],
      moves: Array[String]
  ): Map[Int, mutable.Stack[String]] = {
    moves.foreach(m => {
      val (numberOfMoves, from, to) = parseMove(m)
      //      println(numberOfMoves, from, to)

      var itemsToMove = Seq[String]()
      for (i <- 0 until numberOfMoves) {
//        println(s"----${i}")
        val fromQueue = mapping(from)
        if (fromQueue.nonEmpty) {
          val toMove = fromQueue.pop()
//          println(toMove)
          itemsToMove = itemsToMove :+ toMove
        }

      }
      itemsToMove.reverse.foreach(itm => {
        val toQueue = mapping(to)
        toQueue.prepend(itm)
      })

//      println(mapping.mkString("\n"))

    })
    mapping
  }

  def part2(): String = {
    val input = InputReader.read("day5/input.txt").split("\n\n")
    val (stacks, moves) = input.splitAt(1)
    //     create FIFOS
    val mapping = createStacksMapping(stacks.head.split("\n"))
//    println(mapping.mkString("\n"))
    val mappingAfterMoves =
      makeMovesCrateMover9000(mapping, moves.head.split("\n")).toSeq
        .sortBy(_._1)
//    println(mappingAfterMoves.mkString("\n"))
    val topCrates = mappingAfterMoves
      .map(m => m._2(0).replaceAll("\\[", "").replaceAll("\\]", ""))
      .mkString
//    println(topCrates)
    //    assert(topCrates == "MCD")
    topCrates
  }

  def main(args: Array[String]): Unit = {
    println("Part1")
    println(part1())
    println("Part2")
    println(part2())
  }

}

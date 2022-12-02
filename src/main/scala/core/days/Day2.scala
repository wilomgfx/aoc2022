package core.days

import core.InputReader

object Day2 {

  private val ROCK_MOVE_VALUE = 1
  private val PAPER_MOVE_VALUE = 2
  private val SCISSORS_MOVE_VALUE = 3

  private val WIN_SCORE = 6
  private val DRAW_SCORE = 3

  private def getGameBoard: Array[Array[String]] = {
    val input = InputReader.read("day2/part1.txt").split("\n")
    // [
    // [A, Y],
    // [B, X],
    // [C, Z]
    // ]
    input.map(i => i.split(' ').map(s => s))
  }

  def part1(): Int = {
    val gameBoard = getGameBoard
    var score = 0

    val PAPER = "Y"
    val ROCK = "X"
    val SCISSORS = "Z"

    gameBoard.foreach(gb => {
      val oMove = gb(0)
      val pMove = gb(1)
     (oMove, pMove) match {
       // rock - paper
       case ("A", PAPER) => {
         score += PAPER_MOVE_VALUE + WIN_SCORE
       }
       // rock - rock
       case ("A", ROCK) => {
         score += ROCK_MOVE_VALUE + DRAW_SCORE
       }
       // rock - scissors
       case ("A", SCISSORS) => {
         score += SCISSORS_MOVE_VALUE
       }


       // paper - rock
       case ("B", ROCK) => {
         score += ROCK_MOVE_VALUE
       }
       // paper - paper
       case ("B", PAPER) => {
         score += PAPER_MOVE_VALUE + DRAW_SCORE
       }
       // paper - scissors
       case ("B", SCISSORS) => {
         score += SCISSORS_MOVE_VALUE + WIN_SCORE
       }



       // scissors - scissors
       case ("C", SCISSORS) => {
         score += SCISSORS_MOVE_VALUE + DRAW_SCORE
       }
       // scissors - paper
       case ("C", PAPER) => {
         score += PAPER_MOVE_VALUE
       }
       // scissors - rock
       case ("C", ROCK) => {
         score += ROCK_MOVE_VALUE + WIN_SCORE
       }
     }
    })

    score
//    assert(score == 15)
  }

  def part2(): Int = {
    val gameBoard = getGameBoard
    var score = 0

    val DRAW = "Y"
    val LOSE = "X"
    val WIN = "Z"

    gameBoard.foreach(gb => {
      val oMove = gb(0)
      val pMove = gb(1)
      (oMove, pMove) match {
        // rock - rock
        case ("A", DRAW) => {
          score += ROCK_MOVE_VALUE + DRAW_SCORE
        }
        // rock - paper
        case ("A", LOSE) => {
          score += SCISSORS_MOVE_VALUE
        }
        // rock - paper
        case ("A", WIN) => {
          score += PAPER_MOVE_VALUE + WIN_SCORE
        }


        // paper - rock
        case ("B", LOSE) => {
          score += ROCK_MOVE_VALUE
        }
        // paper - paper
        case ("B", DRAW) => {
          score += PAPER_MOVE_VALUE + DRAW_SCORE
        }
        // paper - scissors
        case ("B", WIN) => {
          score += SCISSORS_MOVE_VALUE + WIN_SCORE
        }



        // scissors - rock
        case ("C", WIN) => {
          score += ROCK_MOVE_VALUE + WIN_SCORE
        }
        // scissors - scissors
        case ("C", DRAW) => {
          score += SCISSORS_MOVE_VALUE + DRAW_SCORE
        }
        // scissors - paper
        case ("C", LOSE) => {
          score += PAPER_MOVE_VALUE
        }
      }
    })

    score
  }

  def main(args: Array[String]) = {
    println("Part1")
    println(part1())
    println("Part2")
    println(part2())
  }

}

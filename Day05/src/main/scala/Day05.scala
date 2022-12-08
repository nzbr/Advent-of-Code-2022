import de.nzbr.aoc22.withFile

import scala.io.Source
import scala.util.matching.Regex

private def withInput = withFile("Input/05.txt")

type State = Array[List[Char]]
case class Instruction(count: Int, from: Int, to: Int)

def parseInput(input: Source): (State, List[Instruction]) = {
  val lines = input.getLines()

  val initialStateLines = lines.takeWhile(_.nonEmpty).toList
  val instructionLines = lines.toList

  val maxLen = initialStateLines.init.map(_.length).max
  val initialState = initialStateLines.last
    .split(" ")
    .filter(_.nonEmpty)
    .map(_.toInt)
    .map(num => {
      initialStateLines.init
        .map(_.padTo(maxLen, ' '))
        .map(_(num * 4 - 3))
        .dropWhile(_ == ' ')
    })

  val instructions = instructionLines
    .map((line) => {
      val matchOpt = new Regex("move (\\d+) from (\\d) to (\\d)").findFirstMatchIn(line)
      assert(matchOpt.isDefined)
      val m = matchOpt.get
      Instruction(m.group(1).toInt, m.group(2).toInt - 1, m.group(3).toInt - 1) // Subtract 1 from from and to to make them compatible with the list indices
    })

  (initialState, instructions)
}

@main def day05(): Unit = {

  withInput((input) => {
    val (initialState, instructions) = parseInput(input)

    // part one
    val singleInstructions = instructions.foldRight(List[Instruction]())((instruction, acc) => {
      (1 to instruction.count).foldLeft(acc)((acc, _) => (instruction :: acc)) // Copy every instruction count times
    })
    println(
      singleInstructions.foldLeft(initialState)((state, instruction) => {
        state
          .updated(instruction.to, state(instruction.from).head :: state(instruction.to))
          .updated(instruction.from, state(instruction.from).tail)
      })
        .map(_.head)
        .mkString("")
    )

    // part two
    println(
      instructions.foldLeft(initialState)((state, instruction) => {
        state
          .updated(instruction.to, state(instruction.from).take(instruction.count) ++ state(instruction.to))
          .updated(instruction.from, state(instruction.from).drop(instruction.count))
      })
        .map(_.head)
        .mkString("")
    )

  })

}

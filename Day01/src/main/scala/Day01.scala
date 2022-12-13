import scala.io.Source

import de.nzbr.aoc22.splitList

@main def day01(): Unit = {
  val inputSrc = Source.fromFile("Input/01.txt")
  lazy val input = inputSrc.getLines()

  lazy val sums =
    splitList(input, "")
      .map(_.map(_.toInt))
      .map(_.sum)

  // part one
  println(
    sums.max
  )

  // part two
  println(
    sums
      .sorted
      .reverse
      .take(3)
      .sum
  )

  // cleanup
  inputSrc.close()
}

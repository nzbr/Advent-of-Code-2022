import de.nzbr.aoc22.withFile

import scala.io.Source

private def withInput = withFile("Input/10.txt")

private def runCPU(input: Source) = {
  input
  .getLines()
    .scanLeft(1, List.empty[Int])((acc, line) => {
      val x = acc._1
      line match {
        case s"noop" => (x, List(x))
        case s"addx ${num}" => (x + num.toInt, List(x, x))
      }
    })
}

@main def day10(): Unit = {

  // part one
  withInput((input) => {
    println(
      runCPU(input)
        .flatMap(_._2)
        .zipWithIndex
        .drop(19)
        .grouped(40)
        .map(_.head)
        .map(it => it._1 * (it._2 + 1)) // Multiply with index, index starts with 0, so add 1
        .sum
    )
  })

  // part two
  withInput((input) => {
    println(
      runCPU(input)
        .flatMap(_._2)
        .grouped(40)
        .map((line) => {
          line.zipWithIndex
            .map(v => if math.abs(v(0) - v(1)) <= 1 then "#" else ".")
            .mkString("")
        })
        .mkString("\n")
    )
  })

}

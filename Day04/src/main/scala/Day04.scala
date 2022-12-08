import de.nzbr.aoc22.withFile

import scala.io.Source

private def withInput = withFile("Input/04.txt")

private def expandRanges(line: String): (Seq[Int], Seq[Int]) = {
  val split = line.split(",") // Split at the comma
  assert(split.length == 2)
  val ranges = split.map((range) => { // Expand the ranges
    val bounds = range.split("-")
    assert(bounds.length == 2)
    bounds(0).toInt to bounds(1).toInt
  })
  (ranges(0), ranges(1))
}

@main def day04(): Unit = {

  // part one
  withInput((input) => {
    println(
      input
        .getLines()
        .map(expandRanges)
        .count((ranges) => // Count how many contain the other
          (ranges(0).min <= ranges(1).min && ranges(0).max >= ranges(1).max)
          ||
          (ranges(1).min <= ranges(0).min && ranges(1).max >= ranges(0).max)
        )
    )
  })

  // part two
  withInput((input) => {
    println(
      input
        .getLines()
        .map(expandRanges)
        .map((ranges) => ranges(0).intersect(ranges(1))) // Find any overlaps
        .filter(_.length > 0) // Filter out any empty ranges
        .length
    )
  })

}

import de.nzbr.aoc22.withFile

private def withInput = withFile("Input/06.txt")

private def findMarker(input: Iterator[Char], length: Int): Int = {
  input
    .scanLeft(List[Char]())((acc, c) => c :: acc.take(length - 1))
    .map(_.distinct)
    .takeWhile(_.length < length)
    .zipWithIndex
    .toList
    .last(1) + 1
}

@main def day06(): Unit = {

  // part one
  withInput((input) => {
    println(
      findMarker(input, 4)
    )
  })

  // part two
  withInput((input) => {
    println(
      findMarker(input, 14)
    )
  })
}

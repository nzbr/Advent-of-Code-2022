import scala.io.Source
import de.nzbr.aoc22.withFile

def getPriority(s: String): Int = {
  assert(s.length == 1)
  val c = s.head
  if (c.isLower) c - 'a' + 1
  else c - 'A' + 27
}

private def withInput = withFile("Input/03.txt")

@main def day03(): Unit = {

  // part one
  withInput((input: Source) => {
    println(
      input
        .getLines()
        .map((line) => line.splitAt(line.length / 2)) // split int compartments
        .map((rucksack) => rucksack(0).intersect(rucksack(1))) // get common items
        .map(_.distinct) // remove duplicates
        .map(getPriority) // get priority
        .sum
    )
  })

  // part two
  withInput((input: Source) => {
    println(
      input
        .getLines()
        .map(_.distinct) // remove duplicates
        .grouped(3) // create groups of 3
        .map(group => group.tail.foldLeft(group.head)((rest, rucksack) => rest.intersect(rucksack))) // find the common item
        .map(getPriority)
        .sum
    )
  })

}

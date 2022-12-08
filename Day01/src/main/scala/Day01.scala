import scala.io.Source

def splitList[T](list: IterableOnce[T], delimiter: T): LazyList[LazyList[T]] = {
  list.iterator.foldLeft(LazyList(LazyList[T]())) { (acc, elem) =>
    if (elem == delimiter) LazyList.empty #:: acc
    else (elem #:: acc.head) #:: acc.tail
  }
}

@main def day01(): Unit = {
  val inputSrc = Source.fromFile("input/01.txt")
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

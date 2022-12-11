import de.nzbr.aoc22.withFile

private def withInput = withFile("Input/08.txt")

def takeWhileAndOne[T](values: IterableOnce[T])(predicate: T => Boolean): LazyList[T] = {
  values.iterator.foldLeft(LazyList.empty[T], true)((acc, value) => {
    if (acc(1)) {
      val appended = acc(0) #::: LazyList(value)
      if (predicate(value)) (appended, true) else (appended, false)
    }
    else acc
  })._1
}

@main def day08(): Unit = {

  withInput((input) => {
    val processedInput = input
      .getLines()
      .map((line) => {
        line
          .split("")
          .map(_.toInt)
          .toList
      })
      .toList

    // part one
    println(
      processedInput
        .zipWithIndex
        .flatMap((line, x) => {
          line
            .zipWithIndex
            .map((tree, y) => {
              x == 0 || y == 0 || x == processedInput.length - 1 || y == line.length - 1 ||
                List(
                  (x + 1 until processedInput.length).map((x1) => (x1, y)),
                  (0 until x).map((x1) => (x1, y)),
                  (y + 1 until processedInput(x).length).map((y1) => (x, y1)),
                  (0 until y).map((y1) => (x, y1))
                )
                  .exists((path) => {
                    path
                      .forall((coords) => {
                        tree > processedInput(coords(0))(coords(1))
                      })
                  })
            })
        })
        .count(identity)
    )

    // part two
    println(
      processedInput
        .zipWithIndex
        .flatMap((line, x) => {
          line
            .zipWithIndex
            .map((tree, y) => {
              List(
                (y + 1 until processedInput(x).length).map((y1) => (x, y1)), // right
                (x + 1 until processedInput.length).map((x1) => (x1, y)), // down
                (y - 1 to 0 by -1).map((y1) => (x, y1)), // left
                (x - 1 to 0 by -1).map((x1) => (x1, y)), // up
              )
                .map((path) => {
                    takeWhileAndOne(path)((coords) => {
                      tree > processedInput(coords(0))(coords(1))
                    })
                    .length
                })
                .product(Numeric.IntIsIntegral)
            })
        })
        .max
    )
  })

}

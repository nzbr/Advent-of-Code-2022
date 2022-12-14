import de.nzbr.aoc22.withFile

private def withInput = withFile("Input/12.txt")

def findValue(list: Seq[Seq[Boolean]]): (Int, Int) = {
  list
    .map((line) => {
      line
        .zipWithIndex
        .filter(_._1)
        .map(_._2)
    })
    .zipWithIndex
    .filter(_._1.nonEmpty)
    .map((line, x) => {
      (x, line.head)
    })
    .head
}

@main def day12(): Unit = {
  withInput((input) => {
    val lines = input.getLines().toList

    val map = lines
      .map((line) => {
        line.map {
          case 'E' => 26
          case 'S' => 1
          case c => c.toInt - 'a'.toInt + 1
        }
      })

    val start = findValue(
      lines.map(_.map(_ == 'S'))
    )
    val end = findValue(
      lines.map(_.map(_ == 'E'))
    )

    var distances: Array[Array[Int]] = Array(Array.empty[Int])
    var queue: Set[(Int, Int)] = Set()
    var visited: Set[(Int, Int)] = Set()

    def init(from: (Int, Int)): Unit = {
      distances = map.map(_.map((_) => Integer.MAX_VALUE).toArray).toArray
      distances(from(0))(from(1)) = 0
      queue = Set(from)
      visited = Set(from)
    }

    def getNext: (Int, Int) = {
      val next = queue.toList.minBy((x, y) => distances(x)(y)) // get unvisited node with lowest distance
      queue -= next
      visited += next
      next
    }

    def neighbors(x: Int, y: Int): List[(Int, Int)] = {
      List(
        (x - 1, y),
        (x, y - 1),
        (x + 1, y),
        (x, y + 1)
      )
        .filter((xn, yn) => xn >= 0 && xn < map.length && yn >= 0 && yn < map(xn).length)
    }

    def updateDistances(nodes: List[(Int, Int)], x: Int, y: Int): Unit = {
      val distanceNew = distances(x)(y) + 1
      nodes.foreach((xn, yn) => {
        if (distanceNew < distances(xn)(yn)) {
          distances(xn)(yn) = distanceNew
        }
      })
    }

    def enqueueDiscovered(nodes: List[(Int, Int)]): Unit = {
      queue ++= nodes.filter(!visited.contains(_))
    }

    // part one
    init(start)
    while (queue.nonEmpty) {
      val (x, y) = getNext

      val reachable = neighbors(x, y).filter((xn, yn) => map(xn)(yn) - map(x)(y) <= 1)
      updateDistances(reachable, x, y)
      enqueueDiscovered(reachable)
    }
    println(
      distances(end(0))(end(1))
    )

    // part two
    init(end) // run the search backwards
    while (queue.nonEmpty) {
      val (x, y) = getNext

      // We're walking backwards, so the operation has to be reversed as well
      val reachable = neighbors(x, y).filter((xn, yn) => map(x)(y) - map(xn)(yn) <= 1)
      updateDistances(reachable, x, y)
      enqueueDiscovered(reachable)
    }
    println(
      map
        .indices.flatMap((x) => map(x).indices.map((y) => (x, y))) // From all possible coordinates
        .filter((x, y) => map(x)(y) == 1) // Where the elevation is 1 (a/S)
        .map((x, y) => distances(x)(y)).min // Get the minimum distance
    )

  })

}

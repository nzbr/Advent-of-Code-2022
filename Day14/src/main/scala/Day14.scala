import de.nzbr.aoc22.*

private def withInput = withFile("Input/14.txt")

def range(a: Int, b: Int): Range = math.min(a, b) to math.max(a, b)

@main def day14(): Unit = {
  withInput((input) => {
    val paths = input
      .getLines()
      .map((line) => {
        //noinspection ZeroIndexToHead
        line.split(" -> ").toList
          .map(_.split(",").toList)
          .map(_.map(_.toInt))
          .map((it) => (it(0), it(1)))
      })
      .toList

    var rock = Set[(Int, Int)]()
    val start = (500, 0)

    paths.map((path) => {
      path
        .zip(path.drop(1))
        .map((s, e) => {
          if (s._1 == e._1) {
            range(s._2, e._2).foreach((y) => rock += (s._1, y))
          } else if (s._2 == e._2) {
            range(s._1, e._1).foreach((x) => rock += (x, s._2))
          } else {
            assert(false)
          }
        })
    })

    val maxy = rock.map(_._2).max

    def generate(x: Int, y: Int): List[(Int, Int)] = List(
      (x, y + 1),
      (x - 1, y + 1),
      (x + 1, y + 1)
    )

    // part one
    var blocked = rock
    var num = 0
    var potential = List.empty[(Int, Int)]
    while {
      var pos = start
      while {
        val (x, y) = pos
        potential = generate(x, y)
          .filterNot(blocked.contains)
        pos._2 < maxy && potential.nonEmpty // condition
      } do {
        pos = potential.head
      }
      if (potential.isEmpty) {
        blocked += pos
        num += 1
      }
      potential.isEmpty // condition
    } do ()
    println(num)

    // part two
    blocked = rock
    val floor = maxy + 2
    num = 0
    while {
      num += 1
      var pos = start
      while {
        val (x, y) = pos
        potential = generate(x, y)
          .filterNot(blocked.contains)
          .filterNot(_._2 >= floor)
        potential.nonEmpty // condition
      } do {
        pos = potential.head
      }
      blocked += pos
      pos != start // condition
    } do ()
    println(num)
  })
}

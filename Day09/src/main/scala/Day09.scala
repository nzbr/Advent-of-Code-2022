import de.nzbr.aoc22.withFile

private def withInput = withFile("Input/09.txt")

class Vector2(val x: Int, val y: Int) extends Comparable[Vector2] {
  def +(other: Vector2): Vector2 = Vector2(x + other.x, y + other.y)

  def -(other: Vector2): Vector2 = Vector2(x - other.x, y - other.y)

  override def compareTo(other: Vector2): Int = {
    if (x.compareTo(other.x) != 0)
    then x.compareTo(other.x)
    else y.compareTo(other.y)
  }

  override def equals(other: Any): Boolean = other match {
    case Vector2(ox, oy) => x == ox && y == oy
    case _ => false
  }

  override def hashCode(): Int = x + y

  override def toString: String = s"($x, $y)"
}

def generateVectors(l: Iterator[String]): Iterator[Vector2] = {
  l.flatMap((line) => {
    val (direction, distanceStr) = line.splitAt(1)
    val distance = distanceStr.trim.toInt
    direction match
      case "R" => List.fill(distance)(Vector2(1, 0))
      case "L" => List.fill(distance)(Vector2(-1, 0))
      case "U" => List.fill(distance)(Vector2(0, 1))
      case "D" => List.fill(distance)(Vector2(0, -1))
  })
}

def drag(head: Vector2, tail: Vector2): Vector2 = {
  val diff = head - tail
  var newtail = tail
  if (diff.x.abs > 1 || diff.y.abs > 1) {
    if (diff.x == 0) {
      newtail += (if (diff.y > 0) then Vector2(0, 1) else Vector2(0, -1))
    } else if (diff.y == 0) {
      newtail += (if (diff.x > 0) then Vector2(1, 0) else Vector2(-1, 0))
    } else if (diff.x > 0) {
      newtail += (if (diff.y > 0) then Vector2(1, 1) else Vector2(1, -1))
    } else { // diff.x < 0
      newtail += (if (diff.y > 0) then Vector2(-1, 1) else Vector2(-1, -1))
    }
  }
  newtail
}

object Vector2 {
  def unapply(v: Vector2): Option[(Int, Int)] = Some((v.x, v.y))
}

@main def day09(): Unit = {

  val origin = Vector2(0, 0)

  // part one
  withInput((input) => {
    println(
      generateVectors(
        input.getLines().iterator
      )
        .foldLeft(Set(origin), origin, origin)((acc, vec) => {
          val (visited: Set[Vector2], oldHead: Vector2, oldTail: Vector2) = acc
          val head = oldHead + vec
          val tail = drag(head, oldTail)
          (visited + tail, head, tail)
        })
        ._1
        .size
    )
  })

  // part two
  withInput((input) => {
    println(
      generateVectors(
        input.getLines().iterator
      )
        .foldLeft(Set(origin), origin, List.fill(9)(origin))((acc, vec) => {
          val (visited, oldHead, oldRope) = acc
          val head = oldHead + vec
          val rope = oldRope.scanLeft(head)((prev, curr) => {
            drag(prev, curr)
          }).drop(1) // the head gets prepended to the list
          (visited + rope.last, head, rope)
        })
        ._1
        .size
    )
  })

}

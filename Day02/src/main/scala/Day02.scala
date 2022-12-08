import scala.io.Source

def calculateScore(game: (String, String)): Int = game match {
  case ("A", "X") => 1 + 3
  case ("A", "Y") => 2 + 6
  case ("A", "Z") => 3 + 0
  case ("B", "X") => 1 + 0
  case ("B", "Y") => 2 + 3
  case ("B", "Z") => 3 + 6
  case ("C", "X") => 1 + 6
  case ("C", "Y") => 2 + 0
  case ("C", "Z") => 3 + 3
}

def transformMove(game: (String, String)): (String, String) = game match {
  case ("A", "X") => ("A", "Z")
  case ("A", "Y") => ("A", "X")
  case ("A", "Z") => ("A", "Y")
  case ("B", "X") => ("B", "X")
  case ("B", "Y") => ("B", "Y")
  case ("B", "Z") => ("B", "Z")
  case ("C", "X") => ("C", "Y")
  case ("C", "Y") => ("C", "Z")
  case ("C", "Z") => ("C", "X")
}

@main def day02(): Unit = {
  val input = Source.fromFile("input/02.txt")

  lazy val tuples = input
    .getLines()
    .map(line => {
      val split = line.split(" ")
      (split(0), split(1))
    })
    .toList // the source can only be iterated once

  // part one
  println(
    tuples
      .map(calculateScore)
      .sum
  )

  // part two
  println(
    tuples
      .map(transformMove)
      .map(calculateScore)
      .sum
  )

  input.close()
}

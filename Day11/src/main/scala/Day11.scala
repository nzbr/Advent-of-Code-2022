import de.nzbr.aoc22.{splitList, withFile}

import scala.io.Source

private def withInput = withFile("Input/11.txt")

case class Monkey(
                   var numInspected: BigInt,
                   var items: List[BigInt],
                   val operation: BigInt => BigInt,
                   val divisor: BigInt,
                   val nextT: Int,
                   val nextF: Int
                 ) {
  def test(i: BigInt): Boolean = i % divisor == 0
}

@main def day11(): Unit = {
  val generateMonkeys = (input: Source) => splitList(input.getLines(), "")
    .map((monkey) => {

      val items = monkey(1)
        .split(":")(1)
        .split(",")
        .map(_.trim)
        .map(BigInt(_))
        .toList

      val opTk = monkey(2)
        .drop("  Operation: new = ".length)
        .split(" ")

      def determineToken(tk: String) = (x: BigInt) => if tk == "old" then x else BigInt(tk)

      val ot1 = determineToken(opTk(0))
      val ot2 = determineToken(opTk(2))
      val operation = (i: BigInt) => if opTk(1) == "*" then ot1(i) * ot2(i) else ot1(i) + ot2(i)

      val divisor = monkey(3).drop("  Test: divisible by ".length).toLong

      val nextT = monkey(4).drop("    If true: throw to monkey ".length).toInt
      val nextF = monkey(5).drop("    If false: throw to monkey ".length).toInt

      Monkey(0, items, operation, divisor, nextT, nextF)
    })

  // part one
  withInput((input) => {
    val monkeys = generateMonkeys(input)

    (1 to 20).foreach((_) => {
      monkeys.foreach((monkey) => {
        val items = monkey.items
        monkey.items = List.empty
        items.map((item) => {
          monkey.numInspected += 1
          val worryLvl = monkey.operation(item) / BigInt(3)
          val next = if monkey.test(worryLvl) then monkey.nextT else monkey.nextF
          monkeys(next).items :+= worryLvl
        })
      })
    })

    println(
      monkeys.map(_.numInspected).sorted.reverse.take(2).product(Numeric.BigIntIsIntegral)
    )
  })

  // part two
  withInput((input) => {
    val monkeys = generateMonkeys(input)

    val modulus = monkeys.map(_.divisor).product(Numeric.BigIntIsIntegral)

    (1 to 10000).foreach((_) => {
      monkeys.foreach((monkey) => {
        val items = monkey.items
        monkey.items = List.empty
        items.map((item) => {
          monkey.numInspected += 1
          val worryLvl = monkey.operation(item) % modulus
          val next = if monkey.test(worryLvl) then monkey.nextT else monkey.nextF
          monkeys(next).items :+= worryLvl
        })
      })
    })

    println(
      monkeys.map(_.numInspected).sorted.reverse.take(2).product(Numeric.BigIntIsIntegral)
    )
  })

}

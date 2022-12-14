import com.fasterxml.jackson.core.JsonFactory
import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.json.JsonMapper
import com.fasterxml.jackson.databind.node.{ArrayNode, JsonNodeFactory}
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import de.nzbr.aoc22.*

import java.io.StringWriter
import java.util
import java.util.Collections
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*

private def withInput = withFile("Input/13.txt")

private val json = JsonMapper.builder().addModule(DefaultScalaModule).build()
private val nodeFactory = new JsonNodeFactory(true)

def isInOrder(a: JsonNode, b: JsonNode): Int = (a, b) match {
  case (a, b) if a.isInt && b.isInt => (b.asInt() - a.asInt()).sign
  case (a, b) if a.isArray && b.isInt => isInOrder(a, new ArrayNode(nodeFactory, Collections.singletonList(b)))
  case (a, b) if a.isInt && b.isArray => isInOrder(new ArrayNode(nodeFactory, Collections.singletonList(a)), b)
  case (a, b) if a.isArray && b.isArray =>
    @tailrec
    def compareLists(x: List[JsonNode], y: List[JsonNode]): Int = {
      if (x.isEmpty && y.nonEmpty) then {
        1
      } else if (x.isEmpty && y.isEmpty) {
        0
      } else if (y.isEmpty) then {
        -1
      } else {
        val current = isInOrder(x.head, y.head)
        if current != 0
        then current
        else compareLists(x.tail, y.tail)
      }
    }

    compareLists(
      a.elements().asScala.toList,
      b.elements().asScala.toList
    )
}

@main def day13(): Unit = {

  // part one
  withInput((input) => {
    println(
      input
        .getLines()
        .filter(_ != "")
        .grouped(2)
        .map((group) => {
          val objects = group.map((list) => {
            json.readTree(list)
          })

          isInOrder(objects.head, objects.last)
        })
        .zipWithIndex
        .map((v, i) => (i + 1, v)) // increment the indices by 1 (and swap the tuple for debugging purposes)
        .filter((_, result) => result >= 0)
        .map(_._1)
        .sum
    )
  })

  // part two
  val dividers = List(
    "[[2]]",
    "[[6]]"
  )

  withInput((input) => {
    println(
      (input.getLines() ++ dividers)
        .filter(_ != "")
        .map(json.readTree)
        .toList
        .sortWith(isInOrder(_, _) >= 0)
        .map((node) => {
          // This is a little cursed, but I didn't foresee that I'd need the string again when doing part one
          val writer = new StringWriter()
          val generator = new JsonFactory().createGenerator(writer)
          json.writeTree(generator, node)
          writer.toString
        })
        .zipWithIndex
        .filter((v, i) => dividers.contains(v))
        .map(_._2 + 1) // increment the index by 1, because they start with 0
        .product(Numeric.IntIsIntegral)
    )
  })

}

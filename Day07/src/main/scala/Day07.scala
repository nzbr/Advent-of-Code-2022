import de.nzbr.aoc22.withFile

import scala.util.matching.Regex

private def withInput = withFile("Input/07.txt")

sealed trait Node {
  val name: String
  val parent: Option[Directory]

  def size: Int

  def path: String = parent match {
    case Some(p) => p.path + "/" + name
    case None => name
  }

  def root: Directory = parent match {
    case Some(p) => p.root
    case None => this match {
      case d: Directory => d
      case _ => throw new IllegalStateException("/ is not a directory")
    }
  }
}

class Directory(val parent: Option[Directory], val name: String) extends Node {
  var children: Map[String, Node] = Map().empty

  def size: Int = children.map((_, node) => node.size).sum

  def getByPath(path: String): Node =
    path
      .split("/")
      .foldLeft(this: Node)((pwd, dir) => {
        dir match {
          case "." => pwd
          case ".." => pwd.parent.get
          case "" => pwd.root
          case _ => pwd match {
            case d: Directory => d.children(dir)
            case _ => throw new Exception(s"${pwd.path} is not a directory")
          }
        }
      })

  def find: Iterable[Node] = children.flatMap((_, node) => node match {
    case d: Directory => LazyList(d) ++ d.find
    case f: File => LazyList(f)
  })

  override def toString: String = s"Directory($children)"
}

case class File(parent: Option[Directory], name: String, size: Int) extends Node {
  override def toString: String = s"File($size)"
}

@main def day07(): Unit = {

  withInput((input) => {
    val fs = input
      .getLines()
      .foldLeft(new Directory(None, ""))((pwd, line) => {
        val fileEntry = new Regex("(\\d+) (.+)")
        line match {
          case s"$$ cd ${dir}" => pwd.getByPath(dir) match {
            case d: Directory => d
            case _ => throw new Exception(s"${pwd.path} is not a directory")
          }
          case s"dir ${name}" => {
            if pwd.children.contains(name)
            then pwd
            else {
              val dir = new Directory(Some(pwd), name)
              pwd.children += (name -> dir)
              pwd
            }
          }
          case fileEntry(size, name) => {
            if pwd.children.contains(name)
            then pwd
            else {
              val file = File(Some(pwd), name, size.toInt)
              pwd.children += (name -> file)
              pwd
            }
          }
          case s"${_}" => pwd
        }
      })
      .root

    // part one
    println(
      fs
        .find
        .flatMap((node) => node match {
          case d: Directory if d.size <= 100000 => Some(d.size)
          case _ => None
        })
        .sum
    )

    // part two
    val disksize = 70000000
    val required = 30000000
    val free = disksize - fs.size
    val minDelete = required - free
    println(
      fs
        .find
        .flatMap((node) => node match {
          case d: Directory if d.size >= minDelete => Some(d.size)
          case _ => None
        })
        .min
    )

  })


}

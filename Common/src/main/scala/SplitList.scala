package de.nzbr.aoc22

def splitList[T](list: IterableOnce[T], delimiter: T): List[List[T]] = {
  list.iterator.foldLeft(List(List[T]())) { (acc, elem) =>
    if (elem == delimiter) acc :+ List.empty
    else acc.init :+ (acc.last :+ elem)
  }
}

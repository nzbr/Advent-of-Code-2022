package de.nzbr.aoc22

import java.io.Closeable
import scala.io.Source

def doWith[T <: Closeable](obj: T)(f: (T) => Unit): Unit = {
  try f(obj)
  finally obj.close()
}

def withFile(file: String)(f: (Source) => Unit): Unit = doWith(Source.fromFile(file))(f)

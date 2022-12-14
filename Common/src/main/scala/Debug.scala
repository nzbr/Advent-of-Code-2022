package de.nzbr.aoc22

// Based on https://github.com/NixOS/nixpkgs/blob/master/lib/debug.nix

def trace[T](msg: Any)(value: T): T = {
  println(s"trace: ${msg}")
  value
}

def traceValFn[T](fn: T => Any)(value: T): T = trace[T](fn(value))(value)

def traceVal[T] = traceValFn[T](identity)

def traceIf[T](predicate: Boolean)(msg: Any)(value: T): T = if predicate then trace(msg)(value) else value

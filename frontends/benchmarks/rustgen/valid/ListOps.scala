/* Copyright 2009-2019 EPFL, Lausanne */
import stainless.lang.error
import stainless.rust._

object ListOps {
  sealed trait List
  case class Nil() extends List
  case class Cons(head: Int, tail: List) extends List

  def length(xs: List): Int =
    xs match {
      case Nil() => 0
      case Cons(_, xs) => 1 + length(xs)
    }

  def head(xs: List): Int =
    xs match {
      case Nil() => error("Cannot take head of empty list")
      case Cons(x, _) => x
    }

  def tail(xs: List): List =
    xs match {
      case Nil() => error("Cannot take tail of empty list")
      case Cons(_, xs) => xs
    }

  def main(): Unit = {
    val xs = Cons(1, Cons(2, Nil()))
    println(length(xs))
    println(head(xs))
    println(head(tail(xs)))
    println(head(tail(tail(xs))))
  }
}
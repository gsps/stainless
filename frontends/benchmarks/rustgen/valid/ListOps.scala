/* Copyright 2009-2019 EPFL, Lausanne */
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

  def main(): Unit = {
    val xs = Nil()
    println(length(xs))
  }
}
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

  def at_least_two(xs: List): Boolean =
    xs match {
      case Cons(_, Cons(_, _)) => true
      case _ => false
    }

  def zip_and_add(xs: List, ys: List): List =
    (xs, ys) match {
      case (Cons(x, xs0), Cons(y, ys0)) => Cons(x + y, zip_and_add(xs0, ys0))
      case _ => Nil()
    }

  def is_sorted(xs: List): Boolean =
    xs match {
      case Cons(x, ys @ Cons(y, _)) => x <= y && is_sorted(ys)
      case _ => true
    }

  def main(): Unit = {
    val xs = Cons(1, Cons(2, Nil()))
    println(length(xs))
    println(head(xs))
    println(head(tail(xs)))
    println(at_least_two(xs))

    val ys = zip_and_add(xs, xs)
    println(length(ys))
    println(head(ys))
    println(is_sorted(ys))
  }
}
/* Copyright 2009-2019 EPFL, Lausanne */
import stainless.rust._

object Factorial {
  def factorial(n: Int): Int = {
    if (n <= 0) 1
    else n * factorial(n-1)
  }

  def main(): Unit = {
    println(factorial(3));
    println(factorial(4))
  }
}
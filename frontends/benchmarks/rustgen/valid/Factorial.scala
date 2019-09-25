/* Copyright 2009-2019 EPFL, Lausanne */

object Factorial {
  def factorial(n: Int): Int = {
    if (n <= 0) 1
    else n * factorial(n-1)
  }
}

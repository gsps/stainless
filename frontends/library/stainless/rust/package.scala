/* Copyright 2009-2019 EPFL, Lausanne */

package stainless

import stainless.annotation._

package object rust {
  @library
  def `println!`(fmt: String): Unit = {}
  @library
  def `println!`(fmt: String, arg1: Int): Unit = {}
  @library
  def `println!`(fmt: String, arg1: Boolean): Unit = {}
  
  @library @inline
  def println(): Unit = `println!`("")
  @library @inline
  def println(arg1: Int): Unit = `println!`("{}", arg1)
  @library @inline
  def println(arg1: Boolean): Unit = `println!`("{}", arg1)
}
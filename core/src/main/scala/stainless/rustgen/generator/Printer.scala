/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package rustgen
package generator

import scala.collection.mutable.{ArrayBuffer, StringBuilder}


class PrinterContext(val printer: Printer, val indentLevel: Int) {
  def inner = new PrinterContext(printer, indentLevel + PrinterContext.indentStep)

  lazy val indent: String = " " * indentLevel
  lazy val newline: String = "\n" + indent
}

object PrinterContext {
  def apply(printer: Printer) = new PrinterContext(printer, 0)
  val indentStep: Int = 2
}

class PrintableChunk private[generator] (implicit val ctx: PrinterContext) {
  private[generator] val lines: ArrayBuffer[(Option[Int], StringBuffer)] =
    ArrayBuffer((None, new StringBuffer))
  
  private def lastBuffer: StringBuffer = lines.last._2

  private def appendNewline(): Unit = {
    lines.append((None, new StringBuffer))
  }

  def appendNewlineFreeString(string: String): Unit = {
    if (string.nonEmpty) {
      val lastIndex = lines.length - 1
      val (lastLevel, lastBuffer) = lines(lastIndex)
      lines(lastIndex) = (lastLevel.orElse(Some(ctx.indentLevel)), lastBuffer)
      lastBuffer.append(string)
    }
  }
  
  private def appendNewlineFreeStringBuffer(level: Option[Int],
      buffer: StringBuffer): Unit = {
    val lastIndex = lines.length - 1
    val (lastLevel, lastBuffer) = lines(lastIndex)
    lines(lastIndex) = (lastLevel.orElse(level), lastBuffer)
    lastBuffer.append(buffer)
  }

  def appendMultiLineString(string: String): Unit = {
    val lines = string.split("\n", -1).iterator
    if (lines.nonEmpty) {
      val line0 = lines.next()
      appendNewlineFreeString(line0)
      for { line <- lines } {
        appendNewline()
        appendNewlineFreeString(line)
      }
    }
  }

  def +=(other: PrintableChunk): Unit = {
    val otherLines = other.lines.iterator
    val (otherLevel, otherBuffer) = otherLines.next()
    appendNewlineFreeStringBuffer(otherLevel, otherBuffer)
    // FIXME: We should clone the very last StringBuffer in otherLines, because
    //        it might be mutated later through methods on `other`.
    lines.appendAll(otherLines)
  }

  override def toString: String = {
    val rootChunk = PrintableChunk.empty
    rootChunk += this

    val buffer = new StringBuffer
    var isFirst = true
    for { (level, lineBuffer) <- rootChunk.lines } {
      if (!isFirst) {
        buffer.append('\n')
      }
      isFirst = false
      buffer.append(" " * level.getOrElse(0))
      buffer.append(lineBuffer)
    }
    buffer.toString
  }
}

object PrintableChunk {
  def empty(implicit ctx: PrinterContext): PrintableChunk =
    new PrintableChunk()
}

object Printer {
  implicit class PrinterStringContext(private val sc: StringContext) extends AnyVal {
    def p(args: Any*)(implicit ctx: PrinterContext): PrintableChunk = {
      val result = PrintableChunk.empty
      val partsIt = sc.parts.iterator
      val argsIt = args.iterator
      result.appendMultiLineString(partsIt.next())
      for { (arg, part) <- argsIt zip partsIt } {
        arg match {
          case string: String        => result.appendMultiLineString(string)
          case id: Identifier        => result.appendNewlineFreeString(id.toString)
          case tree: rust.Tree       => result += ctx.printer.print(tree)
          case chunk: PrintableChunk => result += chunk
          case _ => throw new IllegalArgumentException(
            s"Printer string interpolator does not support: >> $arg <<")
        }
        result.appendMultiLineString(part)
      }
      result
    }
  }

  protected def separated(sep: String, chunks: Seq[PrintableChunk])(
      implicit ctx: PrinterContext): PrintableChunk = {
    val result = PrintableChunk.empty
    val it = chunks.iterator
    if (it.nonEmpty) {
      result += it.next()
      for { chunk <- it } {
        result += p"$sep"
        result += chunk
      }
    }
    result
  }
  protected def commaSeparated(chunks: Seq[PrintableChunk])(
      implicit ctx: PrinterContext): PrintableChunk =
    separated(", ", chunks)
  protected def nlSeparated(chunks: Seq[PrintableChunk], n: Int = 1)(
      implicit ctx: PrinterContext): PrintableChunk =
    separated("\n" * n, chunks)
  protected def commanlSeparated(chunks: Seq[PrintableChunk])(
      implicit ctx: PrinterContext): PrintableChunk =
    separated(",\n", chunks)
}


class Printer {
  import Printer._
  import rust._

  def show(tree: Tree): String = {
    print(tree)(PrinterContext(this)).toString
  }

  def print(tree: Tree)(implicit ctx: PrinterContext): PrintableChunk = {
    tree match {
      case tree: ModuleDef    => print(tree)
      case tree: Enum         => print(tree)
      case tree: EnumVariant  => print(tree)
      case tree: FunDef       => print(tree)
      case tree: ValDef       => print(tree)
      case tree: Type         => print(tree)
      case tree: Expr         => print(tree)
    }
  }

  def print(module: ModuleDef)(implicit ctx: PrinterContext): PrintableChunk = {
    p"""# Stainless-generated module '${module.name}'

${nlSeparated(module.enums.map(print), 2)}

${nlSeparated(module.functions.map(print), 2)}"""
  }

  def print(enm: Enum)(implicit ctx: PrinterContext): PrintableChunk = {
    val inner = ctx.inner
    p"""enum ${enm.id} {
${commanlSeparated(enm.variants.map(print(_)(inner)))}
}"""
  }

  def print(variant: EnumVariant)(implicit ctx: PrinterContext): PrintableChunk = {
    p"${variant.id} { ${commaSeparated(variant.fields.map(print))} }"
  }

  def print(fun: FunDef)(implicit ctx: PrinterContext): PrintableChunk = {
    p"""fn ${fun.id}(${commaSeparated(fun.params.map(print))}) -> ${fun.resultType} {
${print(fun.body)(ctx.inner)}
}"""
  }

  def print(vd: ValDef)(implicit ctx: PrinterContext): PrintableChunk = {
    p"${vd.id}: ${vd.tpe}"
  }

  def print(tpe: Type)(implicit ctx: PrinterContext): PrintableChunk = {
    tpe match {
      case BoolType       => p"bool"
      case U32Type        => p"u32"
      case I32Type        => p"i32"
      case StrType        => p"str"
      case RefType(tpe)   => p"&$tpe"
      case EnumType(enm)  => p"$enm"
      case TupleType(tps) => p"(${commaSeparated(tps.map(print))})"
    }
  }

  def print(expr: Expr)(implicit ctx: PrinterContext): PrintableChunk = {
    expr match {
      case Variable(id) => p"$id"
      case IntLiteral(value, asType) => p"(${value.toString} as $asType)"
      case Let(vd, value, body) =>
        p"let $vd = $value;\n$body"
      case Reference(expr) =>
        p"&($expr)"
      case FunctionInvocation(fun, args) =>
        p"$fun(${commaSeparated(args.map(print))})"
      case MethodInvocation(method, recv, args) =>
        p"$recv.$method(${commaSeparated(args.map(print))})"
      case IfExpr(cond, thenn, elze) =>
        p"""if $cond {
${print(thenn)(ctx.inner)}
} else {
${print(elze)(ctx.inner)}
}"""
    }
  }
}

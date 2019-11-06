/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package rustgen
package ast

import scala.collection.mutable.{ArrayBuffer, Map => MutableMap, StringBuilder}

case class PrinterOptions(
    printTypes: Boolean = false,
    printImplicit: Boolean = false,
    printLibrary: Boolean = false
)
object PrinterOptions {
  val default = PrinterOptions()
}

class PrinterContext(val printer: Printer, val indentLevel: Int) {
  def inner = new PrinterContext(printer, indentLevel + PrinterContext.indentStep)

  lazy val indent: String = " " * indentLevel
  lazy val newline: String = "\n" + indent
}

object PrinterContext {
  def apply(printer: Printer) = new PrinterContext(printer, 0)
  val indentStep: Int = 2
}

class PrintableChunk private[ast] (implicit val ctx: PrinterContext) {
  private[ast] val lines: ArrayBuffer[(Option[Int], StringBuffer)] =
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

  private def appendNewlineFreeStringBuffer(level: Option[Int], buffer: StringBuffer): Unit = {
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

  def isEmpty: Boolean = lines.head._1.isEmpty

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
        // TODO: Find a real solution for the unique-identifier issue below.
        arg match {
          case string: String        => result.appendMultiLineString(string)
          case id: Identifier        => result.appendNewlineFreeString(id.toString.replaceAll("\\$", "__"))
          case tree: Trees.Tree      => result += ctx.printer.print(tree)
          case chunk: PrintableChunk => result += chunk
          case _ =>
            throw new IllegalArgumentException(
              s"Printer string interpolator does not support: >> $arg <<"
            )
        }
        result.appendMultiLineString(part)
      }
      result
    }
  }

  protected def ifNonEmpty[T](
      elements: Iterable[T]
  )(f: => PrintableChunk)(implicit ctx: PrinterContext): PrintableChunk = {
    if (elements.nonEmpty) f else p""
  }

  protected def separated(sep: String, chunks: Seq[PrintableChunk])(
      implicit ctx: PrinterContext
  ): PrintableChunk = {
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
  protected def commaSeparated(
      chunks: Seq[PrintableChunk]
  )(implicit ctx: PrinterContext): PrintableChunk =
    separated(", ", chunks)
  protected def nlSeparated(chunks: Seq[PrintableChunk], n: Int = 1)(
      implicit ctx: PrinterContext
  ): PrintableChunk =
    separated("\n" * n, chunks)
  protected def commanlSeparated(
      chunks: Seq[PrintableChunk]
  )(implicit ctx: PrinterContext): PrintableChunk =
    separated(",\n", chunks)

  protected def nlAround(chunk: PrintableChunk)(
      implicit ctx: PrinterContext
  ): PrintableChunk =
    p"""
$chunk
"""

  protected def nlSeparatedCompact(chunks: Seq[PrintableChunk], n: Int = 1)(
      implicit ctx: PrinterContext
  ): PrintableChunk =
    ifNonEmpty(chunks)(nlAround(nlSeparated(chunks, n)))
  protected def commanlSeparatedCompact(chunks: Seq[PrintableChunk])(
      implicit ctx: PrinterContext
  ): PrintableChunk =
    ifNonEmpty(chunks)(nlAround(commanlSeparated(chunks)))
}

class Printer(opts: PrinterOptions)(implicit symbols: Trees.Symbols) {
  import Printer._
  import Trees._
  import TypingError._

  protected val QUOTE = "\""

  private val opSymCache: MutableMap[Identifier, String] = MutableMap.empty
  protected def opSym(op: Identifier): String =
    opSymCache.getOrElseUpdate(op, {
      symbols.getFunction(op).flags.collectFirst { case OperatorSymbol(sym) => sym }.get
    })

  /* == Printing entire programs and arbitrary trees == */

  def show(program: Program): String = {
    print(program)(PrinterContext(this)).toString
  }

  def show(tree: Tree): String = {
    print(tree)(PrinterContext(this)).toString
  }

  def print(tree: Tree)(implicit ctx: PrinterContext): PrintableChunk = {
    tree match {
      case tree: Definition  => print(tree)
      case tree: Type        => print(tree)
      case tree: Expr        => print(tree)
      case tree: Flag        => print(tree)
      case tree: Pattern     => print(tree)
      case tree: MatchCase   => print(tree)
      case tree: TypingError => print(tree)
    }
  }

  def print(program: Program)(implicit ctx: PrinterContext): PrintableChunk = {
    val Symbols(structs, enums, allFunctions, _) = program.symbols
    val functions =
      allFunctions.values.toSeq.filter(opts.printLibrary || !_.flags.contains(Library))
    val structsC = nlSeparatedCompact(structs.values.toSeq.map(print), 2)
    val enumsC = nlSeparatedCompact(enums.values.toSeq.map(print), 2)
    val functionsC = nlSeparatedCompact(functions.map(print), 2)
    p"""// Stainless-generated module
#![allow(non_snake_case,unused_parens,unreachable_code,unreachable_patterns)]
use std::rc::Rc;
${structsC}${enumsC}${functionsC}"""
  }

  /* == Printing definitions == */

  def print(defn: Definition)(implicit ctx: PrinterContext): PrintableChunk = {
    defn match {
      case tree: StructDef   => print(tree)
      case tree: EnumDef     => print(tree)
      case tree: EnumVariant => print(tree)
      case tree: FunDef      => print(tree)
      case tree: ValDef      => print(tree)
    }
  }

  def print(struct: StructDef)(implicit ctx: PrinterContext): PrintableChunk = {
    val inner = ctx.inner
    val fields = commanlSeparatedCompact(struct.fields.map(print(_)(inner)))
    p"""struct ${struct.id} {$fields}"""
  }

  def print(enm: EnumDef)(implicit ctx: PrinterContext): PrintableChunk = {
    val inner = ctx.inner
    val variants = commanlSeparatedCompact(enm.variants.map(print(_)(inner)))
    p"""enum ${enm.id} {$variants}"""
  }

  def print(variant: EnumVariant)(implicit ctx: PrinterContext): PrintableChunk = {
    p"${variant.id} { ${commaSeparated(variant.fields.map(print))} }"
  }

  def print(fun: FunDef)(implicit ctx: PrinterContext): PrintableChunk = {
    p"""fn ${fun.id}(${commaSeparated(fun.params.map(print))}) -> ${fun.returnType} {
${print(fun.body)(ctx.inner)}
}"""
  }

  def print(vd: ValDef)(implicit ctx: PrinterContext): PrintableChunk = {
    val ValDef(id, tpe, flags) = vd
    if (flags.isEmpty) {
      p"$id: $tpe"
    } else {
      val result = p"($id: $tpe)"
      for (flag <- flags) result += p" $flag"
      result
    }
  }

  def print(flag: Flag)(implicit ctx: PrinterContext): PrintableChunk = {
    flag match {
      case Library             => p"@library"
      case RefBinding          => p"@refBinding"
      case OperatorSymbol(sym) => p"@operatorSymbol($sym)"
    }
  }

  /* == Conditional parenthesization == */

  private def isComparisonOp(op: Identifier): Boolean = {
    import RustLibrary._
    op == cmp.eq /*|| op == cmp.ne*/ || op == cmp.lt || op == cmp.le || op == cmp.gt || op == cmp.ge
  }

  private def stripImplicit(e: Expr): Expr =
    if (opts.printImplicit)
      e
    else
      e match {
        case Reference(arg, true)   => stripImplicit(arg)
        case Dereference(arg, true) => stripImplicit(arg)
        case _                      => e
      }

  // Operator precedence and associativity for the subset of operators we produce.
  // Also see [https://doc.rust-lang.org/reference/expressions.html#expression-precedence]
  private def precedence(e: Expr): Int = {
    import RustLibrary._
    e match {
      case _: Variable | _: FunctionInvocation | _: MethodInvocation | _: TupleSelect | _: Tuple |
          _: Struct | _: Enum | _: Literal[_] | _: RcNew | _: RcClone | _: RcAsRef =>
        0
      case UnaryOperatorInvocation(ops.neg | ops.not, _) | _: Reference | _: Dereference => 1
      case BinaryOperatorInvocation(ops.div | ops.mul | ops.rem, _, _)                   => 2
      case BinaryOperatorInvocation(ops.add | ops.sub, _, _)                             => 3
      case BinaryOperatorInvocation(ops.shl /*| ops.shr*/, _, _)                         => 4
      case BinaryOperatorInvocation(ops.bitAnd, _, _)                                    => 5
      case BinaryOperatorInvocation(ops.bitXor, _, _)                                    => 6
      case BinaryOperatorInvocation(ops.bitOr, _, _)                                     => 7
      case BinaryOperatorInvocation(op, _, _) if isComparisonOp(op)                      => 8
      case And(_)                                                                        => 9
      case Or(_)                                                                         => 10
      case _                                                                             => 11
    }
  }
  private def isLeftAssoc(e: Expr): Boolean = e match {
    case BinaryOperatorInvocation(op, _, _) if isComparisonOp(op) => false
    case _                                                        => true
  }
  private def isRightAssoc(e: Expr): Boolean = false

  private def unaryOperandNeedsParens(outer: Expr, inner: Expr): Boolean =
    precedence(outer) < precedence(inner)
  private def binaryOperandNeedsParens(outer: Expr, inner: Expr, isLeft: Boolean): Boolean = {
    // Precondition: `outer` is a binary operation
    val (pOuter, pInner) = (precedence(outer), precedence(inner))
    if (pOuter < pInner) true
    else if (pOuter == pInner) isLeft && !isLeftAssoc(outer) || !isLeft && !isRightAssoc(outer)
    else false
  }

  private def parens(chunk: PrintableChunk)(implicit ctx: PrinterContext) = p"($chunk)"

  // Add parentheses to operand, if necessary.
  private def operand(outer: Expr, inner: Expr)(implicit ctx: PrinterContext) =
    if (unaryOperandNeedsParens(outer, stripImplicit(inner))) parens(print(inner)) else print(inner)
  private def leftOperand(outer: Expr, inner: Expr)(implicit ctx: PrinterContext) =
    if (binaryOperandNeedsParens(outer, stripImplicit(inner), isLeft = true)) parens(print(inner))
    else print(inner)
  private def rightOperand(outer: Expr, inner: Expr)(implicit ctx: PrinterContext) =
    if (binaryOperandNeedsParens(outer, stripImplicit(inner), isLeft = false)) parens(print(inner))
    else print(inner)

  /* == Type arguments and constructors == */

  private def typeArgs(tps: Seq[Type])(implicit ctx: PrinterContext) =
    ifNonEmpty(tps)(p"::<${commaSeparated(tps.map(print))}>")

  private def genericType(id: Identifier, tps: Seq[Type])(
      implicit ctx: PrinterContext
  ): PrintableChunk =
    p"$id${typeArgs(tps)}"

  private def enumVariant(id: Identifier, tps: Seq[Type])(implicit ctx: PrinterContext) = {
    val vari = symbols.getEnumVariant(id)
    p"${genericType(vari.enm, tps)}::$id"
  }

  /* == Printing expressions and types == */

  /* Annotate a chunk with a type for debugging purposes */
  private def decorateWithType(chunk: PrintableChunk, tpe: => Type)(implicit ctx: PrinterContext) =
    if (opts.printTypes) p"($chunk : $tpe)" else chunk

  def print(err: TypingError)(implicit ctx: PrinterContext): PrintableChunk = {
    err match {
      case ArgumentTypeMismatch(blamed, param, actual) =>
        p"argument/rhs type $actual doesn't match parameter $param"
      case ArityMismatch(blamed, expected, actual) =>
        p"expected ${expected.tps.size} arguments, but got ${actual.tps.size}"
      case ReturnTypeMismatch(blamed, expected, actual) =>
        p"returned type $actual doesn't match result type $expected"
      case ConditionTypeMismatch(blamed, actual) =>
        p"type $actual doesn't match expected boolean type"
      case MergeTypeMismatch(blamed, expected, actual) =>
        p"branch result type $actual doesn't match expected type $expected"
      case PatternBinderMismatch(blamed, expected) =>
        p"pattern binder $blamed doesn't match scrutinee type $expected"
      case PatternTypeMismatch(blamed, expected) =>
        p"pattern $blamed doesn't match scrutinee type $expected"
      case TypeMatchMismatch(blamed, actual, expected) =>
        p"type $actual doesn't match shape $expected"
    }
  }

  def print(tpe: Type)(implicit ctx: PrinterContext): PrintableChunk = {
    tpe match {
      case NoType              => p"<none>"
      case ErrorType(reason)   => p"<error: ${print(reason)}>"
      case HoleType(id)        => p"?$id"
      case UnitType()          => p"()"
      case BoolType()          => p"bool"
      case U32Type()           => p"u32"
      case I32Type()           => p"i32"
      case StrType()           => p"str"
      case StructType(id, tps) => genericType(id, tps)
      case EnumType(id, tps)   => genericType(id, tps)
      case TupleType(tps)      => p"(${commaSeparated(tps.map(print))})"
      case RefType(tpe)        => p"&$tpe"
      case RcType(tpe)         => p"Rc<$tpe>"
    }
  }

  def print(expr: Expr)(implicit ctx: PrinterContext): PrintableChunk = {
    val result = expr match {
      case MissingExpr(tpe) => p"<empty tree : $tpe>"

      case Variable(id, _, _) => p"$id"

      case expr: Literal[_] => print(expr)

      case Struct(id, tps, args) =>
        val inner = ctx.inner
        p"""${genericType(id, tps)} {
${commanlSeparated(args.map(print(_)(inner)))}
}"""
      case Enum(id, tps, args) =>
        val inner = ctx.inner
        val vari = symbols.getEnumVariant(id)
        val fieldChunks = (vari.fields zip args) map {
          case (field, arg) =>
            implicit val ctx: PrinterContext = inner
            p"${field.id}: $arg"
        }
        p"""${enumVariant(id, tps)} {
${commanlSeparated(fieldChunks)}
}"""

      case Tuple(exprs)                => p"(${commaSeparated(exprs.map(print))})"
      case TupleSelect(expr, index, _) => p"$expr.${index.toString}"

      case Let(vd, value, body) =>
        p"""let $vd = {
${print(value)(ctx.inner)}
};
$body"""

      case expr: MatchExpr =>
        val MatchExpr(scrutinee, cases) = expr
        val inner = ctx.inner
        p"""match $scrutinee {
${commanlSeparated(cases.map(print(_)(inner)))}
}"""

      case FunctionInvocation(fun, args) =>
        p"$fun(${commaSeparated(args.map(print))})"
      case MethodInvocation(method, recv, args) =>
        p"${operand(expr, recv)}.$method(${commaSeparated(args.map(print))})"
      case UnaryOperatorInvocation(op, arg) =>
        p"${opSym(op)}${operand(expr, arg)}"
      case BinaryOperatorInvocation(op, arg1, arg2) =>
        p"${leftOperand(expr, arg1)} ${opSym(op)} ${rightOperand(expr, arg2)}"

      case And(exprs) =>
        p"${separated(" && ", exprs.map(operand(expr, _)))}"
      case Or(exprs) =>
        p"${separated(" || ", exprs.map(operand(expr, _)))}"

      case IfExpr(cond, thenn, elze) =>
        p"""if $cond {
${print(thenn)(ctx.inner)}
} else {
${print(elze)(ctx.inner)}
}"""

      case Error(_, description) => p"panic!(${StrLiteral(description)})"

      case LabelledBlock(label, body) => p"""'${label.id}: loop { break {
${print(body)(ctx.inner)}
} }"""
      case Break(label, tpe, arg)     => p"break '${label.id} $arg"
      case Sequence(expr1, expr2)     => p"""$expr1;
$expr2"""

      case Reference(arg, false) => p"&${operand(expr, arg)}"
      case Reference(arg, true) =>
        if (opts.printImplicit) p"imp:&${operand(expr, arg)}" else p"$arg"
      case Dereference(arg, false) => p"*${operand(expr, arg)}"
      case Dereference(arg, true) =>
        if (opts.printImplicit) p"imp:*${operand(expr, arg)}" else p"$arg"

      case RcNew(arg)   => p"Rc::new($arg)"
      case RcClone(arg) => p"${operand(expr, arg)}.clone()"
      case RcAsRef(arg) => p"${operand(expr, arg)}.as_ref()"
    }
    decorateWithType(result, expr.getType)
  }

  def print[T](lit: Literal[T])(implicit ctx: PrinterContext): PrintableChunk = {
    lit match {
      case UnitLiteral()             => p"()"
      case BoolLiteral(value)        => p"${value.toString}"
      case IntLiteral(value, asType) => p"(${value.toString} as $asType)"
      case StrLiteral(value)         => p"$QUOTE$value$QUOTE"
    }
  }

  def print(cse: MatchCase)(implicit ctx: PrinterContext): PrintableChunk = {
    val MatchCase(pattern, optGuard, rhs) = cse
    val optGuardChunk = optGuard.map(guard => p"if $guard").getOrElse(p"")
    p"""${print(pattern)}$optGuardChunk => {
${print(rhs)(ctx.inner)}
}"""
  }

  def print(pat: Pattern)(implicit ctx: PrinterContext): PrintableChunk = {
    val chunk = pat match {
      case WildcardPattern(_) =>
        p"_"
      case LiteralPattern(_, lit) =>
        print(lit)
      case StructPattern(_, id, subPatterns) =>
        val vari = symbols.getEnumVariant(id)
        val fieldChunks = (vari.fields zip subPatterns) map {
          case (field, pat) =>
            p"${field.id}: ${print(pat)}"
        }
        p"${vari.enm}::$id { ${commaSeparated(fieldChunks)} }"
      case TuplePattern(_, subPatterns) =>
        p"(${commaSeparated(subPatterns.map(print))})"
    }
    pat.binder map { vd =>
      val ref = if (vd.flags.contains(RefBinding)) p"ref " else p""
      val binderC = p"${ref}${vd.id}"
      p"${decorateWithType(binderC, vd.tpe)} @ $chunk"
    } getOrElse (chunk)
  }
}
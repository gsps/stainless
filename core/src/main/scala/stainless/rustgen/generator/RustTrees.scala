/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package rustgen
package generator

import scala.collection.mutable.StringBuilder


class ItemIdentifier private[generator](id: Identifier)
  extends inox.Identifier(id.name, id.globalId, id.id, alwaysShowUniqueID = false)

object ItemIdentifier {
  private[generator] def apply(name: String): ItemIdentifier =
    new ItemIdentifier(inox.FreshIdentifier(name))
}


object rust {
  type Identifier = ItemIdentifier
  val NoIdentifier = ItemIdentifier("<none>")

  trait Tree {
    def show: String = Printer.show(this)
  }

  case class Program(modules: Seq[ModuleDef]) extends Tree

  case class ModuleDef(
      name: Identifier, parent: Identifier,
      enums: Seq[Enum], functions: Seq[FunDef]) extends Tree {
    override def toString: String = 
      if (parent ne NoIdentifier) s"(...)::$parent::$name" else s"::$name"
  }
  object NoModule extends ModuleDef(null, NoIdentifier, Seq.empty, Seq.empty)
  
  // case class QualifiedName(module: Module, name: String) {
  //   override def toString: String = s"$module::$name"
  // }

  
  case class Enum(id: Identifier, variants: Seq[EnumVariant]) extends Tree
  case class EnumVariant(id: Identifier, enm: Identifier, fields: Seq[ValDef]) extends Tree

  case class FunDef(id: Identifier, params: Seq[ValDef], resultType: Type, body: Expr) extends Tree
  case class ValDef(id: Identifier, tpe: Type) extends Tree


  sealed trait Type extends Tree
  
  sealed trait PrimitiveType extends Type { def rustName: String }
  object BoolType extends PrimitiveType { def rustName = "bool" }
  object U32Type extends PrimitiveType { def rustName = "u32" }
  object I32Type extends PrimitiveType { def rustName = "i32" }
  object StrType extends PrimitiveType { def rustName = "str" }

  case class RefType(tpe: Type) extends Type
  case class EnumType(enm: Enum) extends Type
  case class TupleType(tps: Seq[Type]) extends Type


  sealed trait Expr extends Tree
  
  case class Variable(id: Identifier) extends Expr

  case class IntLiteral(value: Int, asType: Type) extends Expr

  case class Let(vd: ValDef, value: Expr, body: Expr) extends Expr
  
  case class FunctionInvocation(fun: Identifier, args: Seq[Expr]) extends Expr
  case class MethodInvocation(method: Identifier, recv: Expr, args: Seq[Expr]) extends Expr
  // case class UnaryOperatorInvocation(op: Identifier, arg: Expr) extends Expr
  // case class BinaryOperatorInvocation(op: Identifier, arg1: Expr, arg2: Expr) extends Expr
  
  case class IfExpr(cond: Expr, thenn: Expr, elze: Expr) extends Expr


  private def op(rustName: String): Identifier = ItemIdentifier(rustName)

  object stdOps {
    val add = op("add")
    val div = op("div")
    val mul = op("mul")
    val neg = op("neg")
    val not = op("not")
    val rem = op("rem")
    val sub = op("sub")
  }

  object stdCmp {
    val lt = op("lt")
    val le = op("le")
    val gt = op("gt")
    val ge = op("ge")
    val eq = op("eq")
    val ne = op("ne")  // unused!
  }
}


class PrinterContext(builder: StringBuilder, val indentLevel: Int) {
  def inner = new PrinterContext(builder, indentLevel + PrinterContext.indentStep)

  lazy val indent: String = " " * indentLevel
  lazy val newline: String = "\n" + indent
  
  def appendSimpleString(string: String): Unit = {
    // builder.append(indent)
    builder.append(string)
  }
  def appendMultiLineString(string: String): Unit = {
    println("<< " + string.split('\n').mkString(indent, newline, "") + " >>")
    builder.append(string.split('\n').mkString(indent, newline, ""))
  }

  def asString: String = builder.toString
}

object PrinterContext {
  def fresh: PrinterContext = new PrinterContext(new StringBuilder, 0)
  val indentStep: Int = 2
}

object Printer {
  import rust._

  def show(tree: Tree): String = {
    implicit val ctx = PrinterContext.fresh
    print(tree)
    ctx.asString
  }

  def print(tree: Tree)(implicit ctx: PrinterContext): Unit = {
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

  def print(module: ModuleDef)(implicit ctx: PrinterContext): Unit = {
    p"""# Stainless-generated module '${module.name}'\n"""
    nlSeparated(module.enums, 2)(print)
    nlSeparated(module.functions, 2)(print)
  }

  def print(enm: Enum)(implicit ctx: PrinterContext): Unit = {
    val inner = ctx.inner
    p"""enum ${enm.id} {\n"""
    commanlSeparated(enm.variants)(print(_)(inner))
    p"""\n}"""
  }

  def print(variant: EnumVariant)(implicit ctx: PrinterContext): Unit = {
    p"""${variant.id} { """
    variant.fields.foreach(print)
    p""" }"""
  }

  def print(fun: FunDef)(implicit ctx: PrinterContext): Unit = {
    p"""fn ${fun.id}("""
    fun.params.foreach(print)
    p""") -> ${fun.resultType} {\n"""
    print(fun.body)(ctx.inner)
    p"""\n}"""
  }

  def print(vd: ValDef)(implicit ctx: PrinterContext): Unit = {
    p"""${vd.id}: ${vd.tpe}"""
  }

  def print(tpe: Type)(implicit ctx: PrinterContext): Unit = {
    tpe match {
      case BoolType       => p"""bool"""
      case U32Type        => p"""u32"""
      case I32Type        => p"""i32"""
      case StrType        => p"""str"""
      case RefType(tpe)   => p"""&$tpe"""
      case EnumType(enm)  => p"""$enm"""
      case TupleType(tps) =>
        p"""("""
        commaSeparated(tps)(print)
        p""")"""
    }
  }

  def print(expr: Expr)(implicit ctx: PrinterContext): Unit = {
    expr match {
      case Variable(id) => p"""$id"""
      case IntLiteral(value, asType) => p"""(${value.toString} as $asType)"""
      case Let(vd, value, body) =>
        p"""let $vd = $value;\n$body"""
        case FunctionInvocation(fun, args) =>
        p"""$fun("""
        commaSeparated(args)(print)
        p""")"""
        case MethodInvocation(method, recv, args) =>
        p"""$recv.$method("""
        commaSeparated(args)(print)
        p""")"""
      case IfExpr(cond, thenn, elze) =>
        p"""if $cond {\n"""
        print(thenn)(ctx.inner)
        p"""\n} else {\n"""
        print(elze)(ctx.inner)
        p"""\n}"""
    }
  }
  

  implicit class PrinterHelper(private val sc: StringContext) extends AnyVal {
    def p(args: Any*)(implicit ctx: PrinterContext): Unit = {
      for {
        arg <- args
      } arg match {
        case string: String => ctx.appendMultiLineString(string)
        case id: Identifier => ctx.appendSimpleString(id.toString)
        case tree: Tree     => Printer.print(tree)
        case _ => throw new IllegalArgumentException(
          "Printer string interpolator only supports Strings and Trees")
      }
    }
  }

  private def separated[T](sep: String, elements: Seq[T])(f: T => Unit)(
      implicit ctx: PrinterContext): Unit = {
    var isFirst = true
    for { element <- elements } {
      if (!isFirst)
        p"$sep"
      isFirst = false
      f(element)
    }
  }
  private def commaSeparated[T](elements: Seq[T])(f: T => Unit)(
      implicit ctx: PrinterContext): Unit =
    separated(", ", elements)(f)
  private def nlSeparated[T](elements: Seq[T], n: Int = 1)(f: T => Unit)(
      implicit ctx: PrinterContext): Unit =
    separated("\n" * n, elements)(f)
  private def commanlSeparated[T](elements: Seq[T])(f: T => Unit)(
      implicit ctx: PrinterContext): Unit =
    separated(",\n", elements)(f)
}

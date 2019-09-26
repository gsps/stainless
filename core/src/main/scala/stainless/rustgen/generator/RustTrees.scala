/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package rustgen
package generator


class ItemIdentifier private[generator](id: Identifier)
  extends inox.Identifier(id.name, id.globalId, id.id, alwaysShowUniqueID = false)

object ItemIdentifier {
  private[generator] def apply(name: String): ItemIdentifier =
    new ItemIdentifier(inox.FreshIdentifier(name))
}


object rust {
  type Identifier = ItemIdentifier
  val Identifier = ItemIdentifier

  trait Tree {
    def show: String = new Printer().show(this)
  }

  case class Program(modules: Seq[ModuleDef]) extends Tree

  case class ModuleDef(
      name: Identifier, parent: Option[Identifier],
      enums: Seq[Enum], functions: Seq[FunDef]) extends Tree {
    override def toString: String =
      parent.map(p => s"(...)::$p::$name").getOrElse(s"::$name")
  }

  
  case class Enum(id: Identifier, variants: Seq[EnumVariant]) extends Tree
  case class EnumVariant(id: Identifier, enm: Identifier, fields: Seq[ValDef]) extends Tree

  case class FunDef(id: Identifier, params: Seq[ValDef], resultType: Type, body: Expr) extends Tree
  case class ValDef(id: Identifier, tpe: Type) extends Tree


  sealed trait Type extends Tree
  
  sealed trait PrimitiveType extends Type
  object UnitType extends PrimitiveType
  object BoolType extends PrimitiveType
  object U32Type extends PrimitiveType
  object I32Type extends PrimitiveType
  object StrType extends PrimitiveType

  case class RefType(tpe: Type) extends Type
  case class EnumType(enm: Enum) extends Type
  case class TupleType(tps: Seq[Type]) extends Type


  sealed trait Expr extends Tree
  
  case class Variable(id: Identifier) extends Expr

  case class UnitLiteral() extends Expr
  case class IntLiteral(value: Int, asType: Type) extends Expr
  case class StrLiteral(value: String) extends Expr

  case class Let(vd: ValDef, value: Expr, body: Expr) extends Expr

  case class Reference(expr: Expr) extends Expr
  
  case class FunctionInvocation(fun: Identifier, args: Seq[Expr]) extends Expr
  case class MethodInvocation(method: Identifier, recv: Expr, args: Seq[Expr]) extends Expr
  // case class UnaryOperatorInvocation(op: Identifier, arg: Expr) extends Expr
  // case class BinaryOperatorInvocation(op: Identifier, arg1: Expr, arg2: Expr) extends Expr
  
  case class IfExpr(cond: Expr, thenn: Expr, elze: Expr) extends Expr


  private def op(rustName: String): Identifier = Identifier(rustName)

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

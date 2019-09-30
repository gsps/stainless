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
    def show(implicit symbols: stainless.trees.Symbols): String =
      new Printer().show(this)
  }

  case class Program(modules: Seq[ModuleDef]) extends Tree

  case class ModuleDef(
      name: Identifier, parent: Option[Identifier],
      enums: Seq[EnumDef], functions: Seq[FunDef]) extends Tree {
    override def toString: String =
      parent.map(p => s"(...)::$p::$name").getOrElse(s"::$name")
  }


  // case class StructDef(id: Identifier,  tparams: Seq[Identifier], fields: Seq[ValDef]) extends Tree

  case class EnumDef(id: Identifier, variants: Seq[EnumVariant]) extends Tree
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
  case class StructType(id: Identifier, tps: Seq[Type]) extends Type
  case class EnumType(id: Identifier, tps: Seq[Type]) extends Type
  case class TupleType(tps: Seq[Type]) extends Type


  sealed trait Expr extends Tree
  
  case class Variable(id: Identifier) extends Expr

  sealed trait Literal[+T] extends Expr {
    def value: T
  }
  case class UnitLiteral() extends Literal[Unit] { def value: Unit = () }
  case class IntLiteral[+T : Integral](value: T, asType: Type) extends Literal[T]
  case class StrLiteral(value: String) extends Literal[String]

  case class Struct(id: Identifier, tps: Seq[Type], args: Seq[Expr]) extends Expr
  case class Enum(id: Identifier, tps: Seq[Type], args: Seq[Expr]) extends Expr
  case class Tuple(exprs: Seq[Expr]) extends Expr

  case class Let(vd: ValDef, value: Expr, body: Expr) extends Expr


  sealed trait Pattern {
    def binder: Option[ValDef]
    def subPatterns: Seq[Pattern]
  }
  case class WildcardPattern(binder: Option[ValDef]) extends Pattern {
    val subPatterns = Seq()
  }
  case class LiteralPattern[+T](binder: Option[ValDef], lit: Literal[T]) extends Pattern {
    val subPatterns = Seq()
  }
  case class StructPattern(binder: Option[ValDef], id: Identifier, subPatterns: Seq[Pattern])
    extends Pattern
  case class TuplePattern(binder: Option[ValDef], subPatterns: Seq[Pattern]) extends Pattern

  case class MatchCase(pattern: Pattern, optGuard: Option[Expr], rhs: Expr) extends Tree
  case class MatchExpr(scrutinee: Expr, cases: Seq[MatchCase]) extends Expr


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

  object stdRc {
    val Rc = Identifier("Rc")
    val neu = Identifier("Rc::new")  // TODO: Introduce explicit rust modules?
    val klone = Identifier("clone")
    val as_ref = Identifier("as_ref")

    object RcType {
      def apply(tp: Type): StructType =
        StructType(Rc, Seq(tp))
      def unapply(tp: StructType): Option[Type] =
        if (tp.id eq Rc) Some(tp.tps.head) else None
    }
  }
}

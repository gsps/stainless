/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package rustgen
package generator

import inox.utils.{Lazy, Positioned}

object rust {
  type Identifier = stainless.Identifier
  val Identifier = stainless.FreshIdentifier

  abstract class LookupException(id: Identifier, what: String)
      extends Exception("Lookup failed for " + what + " with symbol `" + id.uniqueName + "`")
  case class StructLookupException(id: Identifier) extends LookupException(id, "struct")
  case class EnumLookupException(id: Identifier) extends LookupException(id, "enum")
  case class FunctionLookupException(id: Identifier) extends LookupException(id, "function")

  /* == Common IR == */

  case class Program(symbols: Symbols) {
    val typer = new Typer(symbols, isRelaxed = true)

    def show(): String =
      new Printer()(symbols).show(this)
  }

  case class Symbols(
      structs: Map[Identifier, StructDef],
      enums: Map[Identifier, EnumDef],
      functions: Map[Identifier, FunDef]
  ) {
    @inline def enumVariants: Map[Identifier, EnumVariant] = _enumVariants.get
    private[this] val _enumVariants: Lazy[Map[Identifier, EnumVariant]] =
      Lazy(enums.values.flatMap(_.variants.map(vari => vari.id -> vari)).toMap)

    def getStruct(id: Identifier): StructDef =
      structs.get(id).getOrElse(throw StructLookupException(id))
    def getEnum(id: Identifier): EnumDef =
      enums.get(id).getOrElse(throw EnumLookupException(id))
    def getEnumVariant(id: Identifier): EnumVariant =
      enumVariants.get(id).getOrElse(throw EnumLookupException(id))
    def getFunction(id: Identifier): FunDef =
      functions.get(id).getOrElse(throw FunctionLookupException(id))
  }

  sealed trait Tree extends Positioned {
    def copiedFrom(other: Positioned): this.type =
      setPos(other)

    def show(implicit symbols: Symbols): String =
      new Printer().show(this)
  }

  /* Flags */

  sealed abstract class Flag(val name: String) extends Tree

  object Library extends Flag("library")

  /* Definitions */

  sealed trait Definition extends Tree

  case class StructDef(id: Identifier, fields: Seq[ValDef]) extends Definition

  case class EnumDef(id: Identifier, variants: Seq[EnumVariant]) extends Definition
  case class EnumVariant(id: Identifier, enm: Identifier, fields: Seq[ValDef]) extends Definition

  case class FunDef(
      id: Identifier,
      params: Seq[ValDef],
      returnType: Type,
      body: Expr,
      flags: Seq[Flag]
  ) extends Definition

  case class ValDef(id: Identifier, tpe: Type) extends Definition {
    def toVariable: Variable = Variable(id, tpe)
  }

  /* Types */

  sealed trait Type extends Tree {
    def isError: Boolean =
      this match {
        case _: ErrorType => true
        case _            => false
      }
  }

  object NoType extends Type

  case class ErrorType(reason: TypingError) extends Type

  case class HoleType(id: Identifier) extends Type

  sealed trait TypingError extends Tree {
    val blamed: Tree
    val expected: Type
    val actual: Type
    def toType: Type = ErrorType(this)
  }
  object TypingError {
    case class ArgumentTypeMismatch(blamed: Tree, expected: Type, actual: Type) extends TypingError
    case class ReturnTypeMismatch(blamed: Tree, expected: Type, actual: Type) extends TypingError
    case class LetTypeMismatch(blamed: Tree, expected: Type, actual: Type) extends TypingError
    case class ConditionTypeMismatch(blamed: Tree, actual: Type) extends TypingError {
      val expected = BoolType()
    }
    case class MergeTypeMismatch(blamed: Tree, expected: Type, actual: Type) extends TypingError
    case class PatternTypeMismatch(blamed: Tree, expected: Type) extends TypingError {
      val actual = NoType
    }
    case class TypeMatchMismatch(actual: Type, expected: Type) extends TypingError {
      val blamed = NoType
    }
  }

  sealed abstract class PrimitiveType(val isInt: Boolean) extends Type
  case class UnitType() extends PrimitiveType(false)
  case class BoolType() extends PrimitiveType(false)
  case class U32Type() extends PrimitiveType(true)
  case class I32Type() extends PrimitiveType(true)
  case class StrType() extends PrimitiveType(false)

  case class StructType(id: Identifier, tps: Seq[Type]) extends Type {
    ??? // TODO: Renable once we have support for type parametricity
  }
  case class EnumType(id: Identifier, tps: Seq[Type]) extends Type
  case class TupleType(tps: Seq[Type]) extends Type

  /* Expressions */

  sealed trait Expr extends Tree

  sealed case class MissingExpr(tpe: Type) extends Expr

  case class Variable(id: Identifier, tpe: Type) extends Expr {
    def toVal: ValDef = ValDef(id, tpe)
  }

  sealed trait Literal[+T] extends Expr {
    def value: T
  }
  case class UnitLiteral() extends Literal[Unit] { def value: Unit = () }
  case class BoolLiteral(value: Boolean) extends Literal[Boolean]
  case class IntLiteral[+T: Integral](value: T, asType: PrimitiveType) extends Literal[T] {
    assert(asType.isInt)
  }
  case class StrLiteral(value: String) extends Literal[String]

  case class Struct(id: Identifier, tps: Seq[Type], args: Seq[Expr]) extends Expr {
    assert(tps.isEmpty); ??? // TODO: Renable once we have support for type parametricity
  }
  case class Enum(id: Identifier, tps: Seq[Type], args: Seq[Expr]) extends Expr {
    assert(tps.isEmpty) // TODO
  }
  case class Tuple(exprs: Seq[Expr]) extends Expr

  case class Let(vd: ValDef, value: Expr, body: Expr) extends Expr

  sealed trait Pattern extends Tree {
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

  case class FunctionInvocation(fun: Identifier, args: Seq[Expr]) extends Expr
  case class MethodInvocation(method: Identifier, recv: Expr, args: Seq[Expr]) extends Expr
  // case class UnaryOperatorInvocation(op: Identifier, arg: Expr) extends Expr
  // case class BinaryOperatorInvocation(op: Identifier, arg1: Expr, arg2: Expr) extends Expr

  case class IfExpr(cond: Expr, thenn: Expr, elze: Expr) extends Expr

  // TODO: Remove once we have support for type parametricity
  case class Error(tpe: Type, description: String) extends Expr

  /* == Lower-level IR == */

  case class RefType(tpe: Type) extends Type

  case class Reference(expr: Expr) extends Expr

  /* == Reference-counted boxes == */
  // TODO: Remove once we have support for type parametricity

  case class RcType(tpe: Type) extends Type

  case class RcNew(expr: Expr) extends Expr
  case class RcClone(expr: Expr) extends Expr
  case class RcDeref(expr: Expr) extends Expr

  /* == Rust library symbols == */

  val library = new RustLibrary()
}

/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package rustgen
package ast

import inox.utils.{Lazy, Position, Positioned}

object Trees {
  type Identifier = stainless.Identifier
  val Identifier = stainless.FreshIdentifier

  abstract class LookupException(id: Identifier, what: String)
      extends Exception("Lookup failed for " + what + " with symbol `" + id.uniqueName + "`")
  case class StructLookupException(id: Identifier) extends LookupException(id, "struct")
  case class EnumLookupException(id: Identifier) extends LookupException(id, "enum")
  case class FunctionLookupException(id: Identifier) extends LookupException(id, "function")

  /* == Common IR == */

  case class Program(symbols: Symbols) {
    def show(opts: PrinterOptions = PrinterOptions.default): String =
      new Printer(opts)(symbols).show(this)
  }

  case class Symbols(
      structs: Map[Identifier, StructDef],
      enums: Map[Identifier, EnumDef],
      functions: Map[Identifier, FunDef],
      strictTyping: Boolean
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

    val typer = new Typer(this, isStrict = strictTyping)
  }

  object Symbols {
    def apply(
        structs: Seq[StructDef],
        enums: Seq[EnumDef],
        functions: Seq[FunDef],
        strictTyping: Boolean
    ): Symbols = {
      Symbols(
        structs.map(x => x.id -> x).toMap,
        enums.map(x => x.id -> x).toMap,
        functions.map(x => x.id -> x).toMap,
        strictTyping
      )
    }
  }

  sealed trait Tree extends Positioned {
    def copiedFrom(other: Positioned): this.type =
      setPos(other)

    def show(opts: PrinterOptions = PrinterOptions.default)(implicit symbols: Symbols): String =
      new Printer(opts).show(this)
  }

  /* Flags */

  sealed abstract class Flag(val name: String) extends Tree

  object Library extends Flag("library")
  object RefBinding extends Flag("refBinding")

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

  sealed class ValDef(val v: Variable) extends Definition {
    @inline def id = v.id
    @inline def tpe = v.tpe
    @inline def flags = v.flags

    override def setPos(pos: Position): ValDef.this.type = {
      v.setPos(pos)
      super.setPos(pos)
    }

    @inline def toVariable: Variable = v

    override def equals(that: Any): Boolean = that match {
      case other: ValDef => v.equals(other.v)
      case _             => false
    }
    override def hashCode: Int = v.hashCode

    override def toString: String = s"ValDef($id, $tpe, $flags)"

    def copy(id: Identifier = id, tpe: Type = tpe, flags: Seq[Flag] = flags): ValDef =
      new ValDef(v.copy(id = id, tpe = tpe, flags = flags)).copiedFrom(this)
  }

  object ValDef {
    def fresh(name: String, tpe: Type, alwaysShowUniqueID: Boolean = false) =
      ValDef(Identifier(name, alwaysShowUniqueID), tpe)
    def apply(id: Identifier, tpe: Type, flags: Seq[Flag] = Seq.empty) =
      new ValDef(Variable(id, tpe, flags))
    def unapply(vd: ValDef): Option[(Identifier, Type, Seq[Flag])] = Some((vd.id, vd.tpe, vd.flags))
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
    case class ArgumentTypeMismatch(blamed: Tree, param: ValDef, actual: Type) extends TypingError {
      val expected = param.tpe
    }
    case class ArityMismatch(blamed: Tree, expected: TupleType, actual: TupleType)
        extends TypingError
    case class ReturnTypeMismatch(blamed: Tree, expected: Type, actual: Type) extends TypingError
    case class LetTypeMismatch(blamed: Tree, expected: Type, actual: Type) extends TypingError
    case class ConditionTypeMismatch(blamed: Tree, actual: Type) extends TypingError {
      val expected = BoolType()
    }
    case class MergeTypeMismatch(blamed: Tree, expected: Type, actual: Type) extends TypingError
    case class PatternTypeMismatch(blamed: Tree, expected: Type) extends TypingError {
      val actual = NoType
    }
    case class TypeMatchMismatch(blamed: Tree, actual: Type, expected: Type) extends TypingError
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
  case class TupleType(tps: Seq[Type]) extends Type {
    assert(tps.size > 1)
  }

  /* Expressions */

  sealed trait Expr extends Tree {
    def getType(implicit symbols: Symbols): Type =
      symbols.typer.getType(this)
  }

  sealed case class MissingExpr(tpe: Type) extends Expr

  case class Variable(id: Identifier, tpe: Type, flags: Seq[Flag]) extends Expr {
    def toVal: ValDef = new ValDef(this)
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

  case class Tuple(exprs: Seq[Expr]) extends Expr {
    assert(exprs.size > 1)
  }
  case class TupleSelect(expr: Expr, index: Int, arity: Int) extends Expr {
    assert(arity > 1 && 0 <= index && index < arity)
  }

  case class Let(vd: ValDef, value: Expr, body: Expr) extends Expr

  sealed trait Pattern extends Tree {
    def binder: Option[ValDef]
    def subPatterns: Seq[Pattern]

    private def subBinders = subPatterns.flatMap(_.binders).toSet
    def binders: Set[ValDef] = subBinders ++ binder.toSet
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

  /* == Match-flattened IR == */

  case class LabelledBlock(label: ValDef, body: Expr) extends Expr
  case class Break(label: ValDef, tpe: Type, arg: Expr) extends Expr
  case class Sequence(expr1: Expr, expr2: Expr) extends Expr

  /* == Type-lowered IR == */

  case class RefType(tpe: Type) extends Type

  case class Reference(expr: Expr, isImplicit: Boolean) extends Expr
  case class Dereference(expr: Expr, isImplicit: Boolean) extends Expr

  /* == Reference-counted boxes == */
  // TODO: Remove once we have support for type parametricity

  case class RcType(tpe: Type) extends Type

  case class RcNew(arg: Expr) extends Expr
  case class RcClone(arg: Expr) extends Expr
  case class RcAsRef(arg: Expr) extends Expr
}

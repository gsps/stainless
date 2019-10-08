/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package rustgen
package generator

import ast.{ProgramTransformer, DefinitionTransformer, TreeTransformer}
import ast.Trees._
import ast.Deconstructors.Invocation

class TypeLowering extends ProgramTransformer {
  class AdtTypeLowering(implicit symbols: Symbols) extends DefinitionTransformer {
    type Env = Set[Identifier]
    def initEnv = Set.empty

    override def transform(tpe: Type, env: Env): Type = {
      tpe match {
        case tpe: StructType => RcType(tpe)
        case tpe: EnumType   => RcType(tpe)
        case _               => super.transform(tpe, env)
      }
    }

    private def transformBinderType(substs: Map[ValDef, Expr], expr: Expr): Expr = {
      new TreeTransformer {
        override def transform(expr: Expr): Expr = expr match {
          case v: Variable => substs.getOrElse(v.toVal, super.transform(v))
          case _           => super.transform(expr)
        }
      }.transform(expr)
    }

    override def transform(e: Expr, env: Env): Expr = {
      e match {
        case v: Variable =>
          val vAfter = super.transform(v, env).asInstanceOf[Variable]
          vAfter.tpe match {
            case RcType(_) => RcClone(vAfter).copiedFrom(vAfter)
            case _         => vAfter
          }

        case e: Struct =>
          val eAfter = super.transform(e, env)
          RcNew(eAfter).copiedFrom(e)
        case e: Enum =>
          val eAfter = super.transform(e, env)
          RcNew(eAfter).copiedFrom(e)

        case mtch: MatchExpr =>
          val newEnv = env ++ mtch.cases.flatMap(cd => cd.pattern.binders.map(_.id))
          val mtchAfter @ MatchExpr(scrutAfter, _) =
            super.transform(mtch, newEnv).asInstanceOf[MatchExpr]
          scrutAfter.getType match {
            case RcType(_) =>
              val scrutAsRef = RcAsRef(scrutAfter).copiedFrom(scrutAfter)
              mtchAfter.copy(scrutinee = scrutAsRef).copiedFrom(mtchAfter)
            case _ =>
              mtchAfter
          }

        case _ =>
          super.transform(e, env)
      }
    }
  }

  /*
   * Adapt to reference types:
   *   Whenever we have `t: T` and `&T` is expected, replace by `&t`.
   */
  class RefTypeAdaptation(implicit symbols: Symbols) extends TreeTransformer {
    override def transform(e: Expr): Expr = {
      def adaptToExpected(expr: Expr, expected: Type): Expr = {
        val actual = expr.getType
        (actual, expected) match {
          // TODO: Do the same for dereferencing?
          case (_, RefType(`actual`)) => Reference(expr).copiedFrom(expr)
          case _                      => expr
        }
      }
      def adaptAlways(expr: Expr): Expr = {
        assert(expr.getType.isInstanceOf[RcType])
        Reference(expr).copiedFrom(expr)
      }

      e match {
        case Invocation(fun, args, recons) =>
          val tps = symbols.getFunction(fun).params.map(_.tpe)
          recons((args zip tps) map {
            case (arg, expected) =>
              adaptToExpected(super.transform(arg), expected)
          }).copiedFrom(e)

        case RcClone(arg) =>
          RcClone(adaptAlways(super.transform(arg))).copiedFrom(arg)
        case RcAsRef(arg) =>
          RcAsRef(adaptAlways(super.transform(arg))).copiedFrom(arg)

        case _ =>
          super.transform(e)
      }
    }
  }

  def transform(program: Program): Program = {
    implicit val oldSyms: Symbols = program.symbols
    val adtTypeLowering = new AdtTypeLowering()
    val refTypeAdaptation = new RefTypeAdaptation()

    val structs = oldSyms.structs.values.toSeq.map(adtTypeLowering.transform)
    val enums = oldSyms.enums.values.toSeq.map(adtTypeLowering.transform)
    val functions = oldSyms.functions.values.toSeq map { fd =>
      val fd1 = adtTypeLowering.transform(fd)
      refTypeAdaptation.transform(fd1)
    }
    val newSyms = Symbols(structs, enums, functions, strictTyping = true)
    Program(newSyms)
  }
}

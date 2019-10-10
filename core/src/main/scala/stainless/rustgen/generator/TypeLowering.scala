/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package rustgen
package generator

import ast.{ProgramTransformer, DefinitionTransformer, TreeTransformer}
import ast.Trees._
import ast.Deconstructors.Invocation

/*
 * This pass lowers Stainless' purely reference-based types to Rust's boxed Rc<T> types and also
 * to borrowed references &T, where appropriate.
 *
 * Firstly, we lower `ADTType`s to `RcType`s, rewriting structs, enums and functions.
 * At the same time we introduce `RefType`s, i.e. Rust's type constructor of (immutably) borrowed
 * references. We therefore also automatically insert referencing operations `&t` when we have
 * `t: T`, but `&T` is expected.
 */
class TypeLowering extends ProgramTransformer {
  class AdtTypeLowering(implicit symbols: Symbols) extends DefinitionTransformer {
    type Env = Set[Identifier]
    def initEnv = Set.empty

    override def transform(tpe: Type, env: Env): Type = {
      tpe match {
        case tpe: StructType => RcType(tpe).copiedFrom(tpe)
        case tpe: EnumType   => RcType(tpe).copiedFrom(tpe)
        case _               => super.transform(tpe, env)
      }
    }

    override def transform(vd: ValDef, env: Env): ValDef = {
      def transformBinderType(tpe: Type): Type = {
        tpe match {
          case RcType(_) => RefType(tpe).copiedFrom(tpe)
          case _         => tpe
        }
      }

      val vdAfter = super.transform(vd, env)
      if (env contains vdAfter.id) {
        vdAfter.copy(tpe = transformBinderType(vdAfter.tpe))
      } else {
        vdAfter
      }
    }

    override def transform(e: Expr, env: Env): Expr = {
      def adaptToExpected(expr: Expr, expected: Type): Expr = {
        val actual = expr.getType
        (actual, expected) match {
          // TODO: Do the same for dereferencing?
          case (_, RefType(`actual`)) => Reference(expr).copiedFrom(expr)
          case _                      => expr
        }
      }
      def adaptAndWrapIfRc(expr: Expr)(wrapper: Expr => Expr): Expr = {
        expr.getType match {
          case RcType(_)          => wrapper(Reference(expr).copiedFrom(expr)).copiedFrom(expr)
          case RefType(RcType(_)) => wrapper(expr).copiedFrom(expr)
          case _                  => expr
        }
      }

      e match {
        case v: Variable => adaptAndWrapIfRc(super.transform(v, env))(RcClone)

        case e: Struct   => RcNew(super.transform(e, env)).copiedFrom(e)
        case e: Enum     => RcNew(super.transform(e, env)).copiedFrom(e)

        case mtch: MatchExpr =>
          val newEnv = env ++ mtch.cases.flatMap(cd => cd.pattern.binders.map(_.id))
          val mtchAfter @ MatchExpr(scrutAfter, _) =
            super.transform(mtch, newEnv).asInstanceOf[MatchExpr]
          mtchAfter.copy(scrutinee = adaptAndWrapIfRc(scrutAfter)(RcAsRef)).copiedFrom(mtchAfter)

        case Invocation(fun, args, recons) =>
          // Adapt invocation arguments to `RefType`s
          val tps = symbols.getFunction(fun).params.map(_.tpe)
          recons((args zip tps) map {
            case (arg, expected) =>
              adaptToExpected(transform(arg, env), transform(expected, env))
          }).copiedFrom(e)

        case _ =>
          super.transform(e, env)
      }
    }
  }

  def transform(program: Program): Program = {
    val oldSyms: Symbols = program.symbols
    val adtTypeLowering = new AdtTypeLowering()(oldSyms)
    val structs = oldSyms.structs.values.toSeq.map(adtTypeLowering.transform)
    val enums = oldSyms.enums.values.toSeq.map(adtTypeLowering.transform)
    val functions = oldSyms.functions.values.toSeq.map(adtTypeLowering.transform)

    val newSyms = Symbols(structs, enums, functions, strictTyping = true)
    Program(newSyms)
  }
}

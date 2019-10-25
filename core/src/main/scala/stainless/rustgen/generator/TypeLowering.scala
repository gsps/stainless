/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package rustgen
package generator

import ast.{ProgramTransformer, TypedDefinitionTransformer}
import ast.Trees._

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
  class AdtTypeLowering(implicit symbols: Symbols) extends TypedDefinitionTransformer {
    case class Env(expectedTpe: Type, patBinders: Set[Identifier], inReceiverPosition: Boolean)
        extends EnvWithExpected {
      def withExpected(expectedTpe: Type): Env = this.copy(expectedTpe = expectedTpe)
    }
    def initEnv = Env(noExpectedType, Set.empty, inReceiverPosition = false)

    /* The primary type lowering we are performing in this phase: */
    override def transform(tpe: Type, env: Env): Type = {
      tpe match {
        case tpe: StructType => RcType(tpe).copiedFrom(tpe)
        case tpe: EnumType   => RcType(tpe).copiedFrom(tpe)
        case _               => super.transform(tpe, env)
      }
    }

    /* We also rewrite match scrutinees to references, affecting pattern binder types: */
    override def transform(vd: ValDef, env: Env): ValDef = {
      def transformBinderType(tpe: Type): Type = {
        RefType(tpe).copiedFrom(tpe)
      }

      val vdAfter = super.transform(vd, env)
      if (env.patBinders contains vdAfter.id) {
        vdAfter.copy(tpe = transformBinderType(vdAfter.tpe))
      } else {
        vdAfter
      }
    }

    override def transform(e: Expr, env: Env): Expr = {
      def adaptToExpectedRef(expr: Expr, env: Env): Expr = {
        val actual = expr.getType
        val expected = env.expectedTpe
        val isRecv = env.inReceiverPosition
        (actual, expected) match {
          case (_, RefType(`actual`))   => Reference(expr, isImplicit = isRecv).copiedFrom(expr)
          case (RefType(`expected`), _) => Dereference(expr, isImplicit = isRecv).copiedFrom(expr)
          case _                        => expr
        }
      }
      def adaptAndWrapIfRc(expr: Expr)(wrapper: Expr => Expr): Expr = {
        expr.getType match {
          case RcType(_) =>
            wrapper(Reference(expr, isImplicit = true).copiedFrom(expr)).copiedFrom(expr)
          case RefType(RcType(_)) => wrapper(expr).copiedFrom(expr)
          case _                  => expr
        }
      }
      def adaptScrutinee(expr: Expr): Expr = {
        // We need to ensure that all compound data we match upon is passed as references to the
        // underyling data. This is in order to not move the boxes, but also so that we can
        // then match directly using the "unboxed" struct patterns.
        // We therefore perform the following to adaptations:
        // a) "Shallow" Rc-typed values are unwrapped to references of their underlying data
        //    ("as_ref"). Shallow here means that either the scrutinee itself is Rc-typed, or
        //    it is directly contained within a tuple-typed scrutinee.
        // b) Tuple-typed scrutinees are referenced, so as not to move the tuple into any single
        //    given match.
        def unboxRcs(expr: Expr): Expr =
          expr.getType match {
            case TupleType(tps) =>
              Tuple(expr match {
                case Tuple(args) =>
                  args.map(unboxRcs)
                case _ =>
                  // Note that `expr` must be a variable here by construction in match flattening:
                  assert(expr.isInstanceOf[Variable]) // => duplicating `expr` is cheap
                  val arity = tps.size
                  (0 until arity).map(i => unboxRcs(TupleSelect(expr, i, arity).copiedFrom(expr)))
              }).copiedFrom(expr)
            case _ =>
              adaptAndWrapIfRc(expr)(RcAsRef)
          }

        val unboxed = unboxRcs(expr)
        expr.getType match {
          case TupleType(_) => Reference(unboxed, isImplicit = false).copiedFrom(expr)
          case _            => unboxed
        }
      }

      val nextEnv = env.copy(inReceiverPosition = false)
      val eAfter = e match {
        case v: Variable => adaptAndWrapIfRc(super.transform(v, nextEnv))(RcClone)

        case e: Struct => RcNew(super.transform(e, nextEnv)).copiedFrom(e)
        case e: Enum   => RcNew(super.transform(e, nextEnv)).copiedFrom(e)

        case MethodInvocation(method, recv, args) =>
          val recvAfter = transform(recv, nextEnv.copy(inReceiverPosition = true))
          MethodInvocation(method, recvAfter, args.map(transform(_, nextEnv))).copiedFrom(e)

        case mtch: MatchExpr =>
          val newPatBinders = nextEnv.patBinders ++ mtch.cases.flatMap(
            cd => cd.pattern.binders.map(_.id)
          )
          val mtchAfter @ MatchExpr(scrutAfter, _) =
            super.transform(mtch, nextEnv.copy(patBinders = newPatBinders)).asInstanceOf[MatchExpr]
          mtchAfter.copy(scrutinee = adaptScrutinee(scrutAfter)).copiedFrom(mtchAfter)

        case _ =>
          super.transform(e, nextEnv)
      }
      adaptToExpectedRef(eAfter, env)
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

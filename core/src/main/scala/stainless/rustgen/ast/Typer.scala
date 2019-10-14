/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package rustgen
package ast

import Trees._

import scala.annotation.tailrec

import collection.mutable.{Map => MutableMap}

class Typer(_symbols: Trees.Symbols, isStrict: Boolean) {
  import TypingError._

  protected implicit val symbols: Symbols = _symbols

  /* State */

  protected val fdTypeCache: MutableMap[FunDef, Type] = MutableMap.empty
  protected val exprTypeCache: MutableMap[Expr, Type] = MutableMap.empty

  /* Interface */

  def checkWellTyped(): Seq[Identifier] = {
    symbols.functions.values.collect {
      case fd if getType(fd).isError => fd.id
    }.toSeq
  }

  def getType(fd: FunDef): Type = {
    fdTypeCache.getOrElseUpdate(fd, { computeType(fd) })
  }

  def getType(expr: Expr): Type = {
    exprTypeCache.getOrElseUpdate(expr, { computeType(expr) })
  }

  /* Typing primitives */

  // Erasure of both `RefType`s and `RcType`s (for relaxed typing before `TypeLowering`).
  // TODO: Replace by generic type transform
  protected def fullyErased(tpe: Type): Type = {
    if (isStrict) {
      tpe
    } else {
      tpe match {
        case TupleType(tps) => TupleType(tps.map(fullyErased)).copiedFrom(tpe)
        case RcType(tpe)    => fullyErased(tpe)
        case RefType(tpe)   => fullyErased(tpe)
        case _              => tpe
      }
    }
  }

  // Erasure of top-level `RefType`s only (for pattern typing)
  // TODO: Replace by generic type transform
  protected def refErased(tpe: Type): Type = {
    tpe match {
      case RefType(tpe)   => refErased(tpe)
      case _              => tpe
    }
  }

  protected def conformsTo(actual: Type, expected: Type): Boolean = {
    fullyErased(actual) == fullyErased(expected)
  }

  /* Auxiliary functions that bubble up `ErrorType`s */

  protected def ifWellTyped(e: Expr)(f: Type => Type): Type = {
    getType(e) match {
      case tpe: ErrorType => tpe
      case tpe            => f(tpe)
    }
  }

  protected def ifWellTyped(es: Seq[Expr])(f: Seq[Type] => Type): Type = {
    val maybeTps = es.foldLeft[Either[ErrorType, Seq[Type]]](Right(Seq.empty)) {
      case (f @ Left(_), _) => f
      case (Right(tps), e) =>
        getType(e) match {
          case tpe: ErrorType => Left(tpe)
          case tpe            => Right(tpe +: tps)
        }
    }
    maybeTps.map(tps => f(tps.reverse)).merge
  }

  protected def ifGoodType(tpe: Type)(f: Type => Type): Type = {
    if (tpe.isError) tpe else f(tpe)
  }

  protected def ifGoodTypes(tps: Seq[Type])(f: Seq[Type] => Type): Type = {
    tps.find(_.isError).getOrElse(f(tps))
  }

  /* Matching against types wrt. `conformsTo` */

  protected val `?T` = HoleType(Identifier("T"))

  protected def ifTypeMatch(ground: Type, shape: Type, blamed: Tree)(f: Type => Type): Type = {
    var instT: Option[Type] = None
    def mtch(ground: Type, shape: Type): Boolean = {
      (ground, shape) match {
        // TODO: Support StructType and EnumType here one we have type parametricity
        case (TupleType(tps1), TupleType(tps2)) => (tps1 zip tps2).forall(tt => mtch(tt._1, tt._2))
        case (RefType(tpe1), RefType(tpe2))     => mtch(tpe1, tpe2)
        case (RcType(tpe1), RcType(tpe2))       => mtch(tpe1, tpe2)
        case (tpe1, `?T`) if instT.isDefined    => instT.get == tpe1
        case (tpe1, `?T`)                       => instT = Some(tpe1); true
        case (tpe1, tpe2)                       => conformsTo(tpe1, tpe2)
      }
    }
    if (mtch(ground, shape) && instT.isDefined) f(instT.get)
    else TypeMatchMismatch(blamed, ground, shape).toType
  }

  /* Actual typing rules */

  protected def computeType(fd: FunDef): Type = {
    ifWellTyped(fd.body) { bodyTpe =>
      if (!conformsTo(bodyTpe, fd.returnType))
        ReturnTypeMismatch(fd, fd.returnType, bodyTpe).toType
      else
        fd.returnType
    }
  }

  protected def computeType(expr: Expr): Type = {
    def invocation(expr: Expr, id: Identifier, args: Seq[Expr]): Type = {
      ifWellTyped(args) { argTps =>
        val fd = symbols.getFunction(id)
        // TODO: Also check number of arguments
        val optErrorTpe = (fd.params zip argTps) collectFirst {
          case (param, argTpe) if !conformsTo(argTpe, param.tpe) =>
            ArgumentTypeMismatch(expr, param, argTpe).toType
        }
        optErrorTpe getOrElse fd.returnType
      }
    }

    expr match {
      case MissingExpr(tpe) =>
        tpe
      case Variable(_, tpe, _) =>
        tpe
      case UnitLiteral() =>
        UnitType()
      case BoolLiteral(_) =>
        BoolType()
      case IntLiteral(_, asType) =>
        asType
      case StrLiteral(_) =>
        StrType()
      case Struct(id, tps, _) =>
        ifGoodTypes(tps) { tps =>
          // TODO: Check against expected types
          StructType(id, tps)
        }

      case Enum(id, tps, _) =>
        ifGoodTypes(tps) { tps =>
          // TODO: Check against expected types
          EnumType(symbols.getEnumVariant(id).enm, tps)
        }
      case Tuple(exprs) =>
        ifWellTyped(exprs) { tps =>
          TupleType(tps)
        }

      case Let(vd, value, body) =>
        ifWellTyped(value) { valueTpe =>
          if (!conformsTo(valueTpe, vd.tpe))
            LetTypeMismatch(expr, vd.tpe, valueTpe).toType
          else
            getType(body)
        }

      case FunctionInvocation(fun, args) =>
        invocation(expr, fun, args)
      case MethodInvocation(method, recv, args) =>
        invocation(expr, method, recv +: args)

      case IfExpr(cond, thenn, elze) =>
        ifWellTyped(cond) { condTpe =>
          ifWellTyped(thenn) { thenTpe =>
            ifWellTyped(elze) { elseTpe =>
              if (!conformsTo(condTpe, BoolType()))
                ConditionTypeMismatch(expr, condTpe).toType
              else if (!conformsTo(elseTpe, thenTpe))
                MergeTypeMismatch(expr, thenTpe, elseTpe).toType
              else
                thenTpe
            }
          }
        }

      case expr: MatchExpr =>
        computeType(expr)

      case Error(tpe, _) =>
        tpe

      /* Lower-level IR */

      // NOTE: None of the trees below should exist in programs with relaxed typing semantics.
      // However, during program transformations we may produce such lower-level trees and subsequently
      // call the high-level program's typer to compute their types.

      case Reference(expr) =>
        ifWellTyped(expr) { tpe =>
          RefType(tpe)
        }

      /* Reference-counted boxes */

      case RcNew(arg) =>
        ifWellTyped(arg) { tpe =>
          RcType(tpe)
        }
      case RcClone(arg) =>
        ifWellTyped(arg) { tpe =>
          ifTypeMatch(tpe, RefType(RcType(`?T`)), expr) { argTpe =>
            RcType(argTpe)
          }
        }
      case RcAsRef(arg) =>
        ifWellTyped(arg) { tpe =>
          ifTypeMatch(tpe, RefType(RcType(`?T`)), expr) { argTpe =>
            RefType(argTpe)
          }
        }
    }
  }

  protected def computeType(expr: MatchExpr): Type = {
    val MatchExpr(scrutinee, cases) = expr

    def checkBinder(binder: Option[ValDef], scrutTpe: Type)(
        rest: => Option[TypingError]
    ): Option[TypingError] = {
      binder collect {
        case vd if !conformsTo(vd.tpe, scrutTpe) =>
          Some(LetTypeMismatch(vd, scrutTpe, vd.tpe))
      } getOrElse (rest)
    }

    def checkPattern(pat: Pattern, unerasedScrutTpe: Type): Option[TypingError] = {
      // Note that we simply erase `RefType`s during pattern matching regardless of
      // whether the typer is in strict mode to emulate Rust's matching behavior:
      // Rust's pattern matcher automatically adapts its binding behavior when
      // matching against a reference-typed scrutinee. In effect, matching on a
      // `&T`-typed scrutinee is equivalent to matching on a `T`-typed scrutinee,
      // except that binders in the patterns will become *references* to the matched
      // fields, rather than moving or copying the fields.
      // As part of the `TypeLowering` phase, which introduces `RefType`s, pattern
      // binders' types are therefore also rewritten.
      val scrutTpe = refErased(unerasedScrutTpe)

      def mismatch =
        Some(PatternTypeMismatch(pat, scrutTpe))
      def checkRest(subPatterns: Seq[Pattern], subTps: Seq[Type]) =
        (subPatterns zip subTps)
          .map(t => checkPattern(t._1, t._2))
          .find(_.isDefined)
          .flatten

      pat match {
        case WildcardPattern(binder) =>
          None // wildcard can subsume any expected type

        case LiteralPattern(binder, lit) =>
          checkBinder(binder, scrutTpe) {
            val litTpe = ifWellTyped(lit)(identity)
            assert(!litTpe.isError)
            if (!conformsTo(litTpe, scrutTpe))
              mismatch
            else
              None
          }

        case StructPattern(binder, id, subPatterns) =>
          checkBinder(binder, scrutTpe) {
            scrutTpe match {
              case StructType(structId, tps) =>
                assert(tps.isEmpty)
                val struct = symbols.getStruct(structId)
                if (structId != id || struct.fields.size != subPatterns.size)
                  mismatch
                else
                  checkRest(subPatterns, struct.fields.map(_.tpe))
              case EnumType(enumId, tps) =>
                assert(tps.isEmpty)
                val vari = symbols.getEnumVariant(id)
                if (enumId != vari.enm || vari.fields.size != subPatterns.size)
                  mismatch
                else
                  checkRest(subPatterns, vari.fields.map(_.tpe))
              case _ =>
                mismatch
            }
          }

        case TuplePattern(binder, subPatterns) =>
          checkBinder(binder, scrutTpe) {
            scrutTpe match {
              case TupleType(scrutineeTps) if scrutineeTps.size == subPatterns.size =>
                checkRest(subPatterns, scrutineeTps)
              case _ =>
                mismatch
            }
          }
      }
    }

    def caseType(cse: MatchCase, scrutTpe: Type): Type = {
      checkPattern(cse.pattern, scrutTpe).map(_.toType).getOrElse {
        ifWellTyped(cse.optGuard.getOrElse(BoolLiteral(true))) { guardTpe =>
          if (!conformsTo(guardTpe, BoolType()))
            ConditionTypeMismatch(cse, guardTpe).toType
          else
            getType(cse.rhs)
        }
      }
    }

    ifWellTyped(scrutinee) { scrutTpe =>
      ifGoodTypes(cases.map(cse => caseType(cse, scrutTpe))) { caseTps =>
        val expected = caseTps.head
        val optBadCase = (cases zip caseTps) find {
          case (cse, caseTpe) => !conformsTo(caseTpe, expected)
        }
        optBadCase match {
          case None                 => expected
          case Some((cse, caseTpe)) => MergeTypeMismatch(cse, expected, caseTpe).toType
        }
      }
    }
  }
}

// TODO: Factor out the common parts in transform(Expr, Env) with DefinitionTransformer?
abstract class TypedDefinitionTransformer(implicit val symbols: Symbols) extends DefinitionTransformer {
  trait EnvWithExpected {
    val expectedTpe: Type
    def withExpected(expectedTpe: Type): Env
  }
  type Env <: EnvWithExpected
  def initEnv: Env

  // NOTE: For the purpose of this transformer we consider NoType as a wildcard, that is,
  // any type other than ErrorType conforms to it.
  def noExpectedType: Type = NoType

  override def transform(fd: FunDef): FunDef = {
    val defaultEnv = initEnv

    FunDef(
      transform(fd.id, defaultEnv),
      // fd.tparams map (transform(_, defaultEnv)),
      fd.params map (transform(_, defaultEnv)),
      transform(fd.returnType, defaultEnv),
      transform(fd.body, defaultEnv.withExpected(fd.returnType)),
      fd.flags // fd.flags map (transform(_, defaultEnv))
    ).copiedFrom(fd)
  }

  override def transform(e: Expr, env: Env): Expr = {
    val (ids, vs, es, tps /*, flags*/, builder) = TreeDeconstructor.deconstruct(e)
    val envWithoutExpected = env.withExpected(NoType)
    val expectedTps = getExpectedTypes(e, es, env.expectedTpe)
    assert(es.size == expectedTps.size)

    var changed = false

    val newIds = for (id <- ids) yield {
      val newId = transform(id, envWithoutExpected)
      if (id ne newId) changed = true
      newId
    }

    val newVs = for (v <- vs) yield {
      val vd = v.toVal
      val newVd = transform(vd, envWithoutExpected)
      if (vd ne newVd) changed = true
      newVd.toVariable
    }

    val newEs = for ((e, expectedTpe) <- es.zip(expectedTps)) yield {
      val newE = transform(e, env.withExpected(expectedTpe))
      if (e ne newE) changed = true
      newE
    }

    val newTps = for (tp <- tps) yield {
      val newTp = transform(tp, envWithoutExpected)
      if (tp ne newTp) changed = true
      newTp
    }

    // val newFlags = for (flag <- flags) yield {
    //   val newFlag = transform(flag, envWithoutExpected)
    //   if (flag ne newFlag) changed = true
    //   newFlag
    // }

    if (changed) {
      builder(newIds, newVs, newEs, newTps /*, newFlags*/ ).copiedFrom(e)
    } else {
      e
    }
  }

  protected def getExpectedTypes(expr: Expr, subExprs: Seq[Expr], expectedTpe: Type): Seq[Type] = {
    expr match {
      case Struct(id, tps, _) =>
        // TODO: Get TypedStructDef to ensure field types are instantiated
        assert(tps.isEmpty)
        symbols.getStruct(id).fields.map(_.tpe)
      case Enum(id, tps, _) =>
        // TODO: Get TypedEnumDef to ensure field types are instantiated
        assert(tps.isEmpty)
        symbols.getEnumVariant(id).fields.map(_.tpe)
      case Tuple(args) =>
        expectedTpe match {
          case TupleType(tps) => tps
          case _ => args.map(_ => NoType)
        }
      case Let(vd, _, _) =>
        Seq(vd.tpe, expectedTpe)
      case FunctionInvocation(fun, _) =>
        // TODO: Get TypedFunDef to ensure parameter type are instantiated
        symbols.getFunction(fun).params.map(_.tpe)
      case MethodInvocation(method, _, _) =>
        symbols.getFunction(method).params.map(_.tpe)
      case IfExpr(_, _, _) =>
        Seq(BoolType(), expectedTpe, expectedTpe)

      case MatchExpr(_, cases) =>
        def patTps(pat: Pattern): Seq[Type] = {
          pat match {
            case LiteralPattern(_, _) => Seq(NoType)
            case _ => Seq()
          }
        }
        def caseTps(cse: MatchCase): Seq[Type] = {
          cse.optGuard.map(_ => BoolType()).toSeq ++ Seq(expectedTpe) ++ patTps(cse.pattern)
        }
        Seq(NoType) ++ cases.flatMap(caseTps)

      /* Lower-level IR */

      case Reference(_) =>
        Seq(expectedTpe match {
          case RefType(tpe) => tpe
          case _ => NoType
        })
      case RcNew(_) =>
        Seq(expectedTpe match {
          case RcType(tpe) => tpe
          case _ => NoType
        })
      case RcAsRef(_) =>
        Seq(expectedTpe match {
          case RefType(RcType(tpe)) => expectedTpe
          case _ => NoType
        })

      case _ => Seq.empty
    }
  }
}

/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package rustgen
package generator

import scala.annotation.tailrec

import collection.mutable.{Map => MutableMap}

class Typer(_symbols: rust.Symbols, isRelaxed: Boolean) {
  import rust._
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

  // Erasure for relaxed typing.
  // TODO: Replace by generic type transform
  protected def erased(tpe: Type): Type = {
    if (isRelaxed) {
      tpe match {
        case TupleType(tps) => TupleType(tps.map(erased))
        case RcType(tpe)    => tpe
        case RefType(tpe)   => tpe
        case _              => tpe
      }
    } else {
      tpe
    }
  }

  protected def conformsTo(actual: Type, expected: Type): Boolean = {
    erased(actual) == erased(expected)
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

  protected def ifTypeMatch(ground: Type, shape: Type)(f: Type => Type): Type = {
    var instT: Option[Type] = None
    def mtch(ground: Type, shape: Type): Boolean = {
      (ground, shape) match {
        // TODO: Support StructType and EnumType here one we have type parametricity
        case (TupleType(tps1), TupleType(tps2)) => (tps1 zip tps2).forall(tt => mtch(tt._1, tt._2))
        case (RefType(tpe1), RefType(tpe2))     => mtch(tpe1, tpe2)
        case (RcType(tpe1), RcType(tpe2))       => mtch(tpe1, tpe2)
        case (tpe1, `?T`) if instT.isDefined    => false
        case (tpe1, `?T`)                       => instT = Some(tpe1); true
        case (tpe1, tpe2)                       => conformsTo(tpe1, tpe2)
      }
    }
    if (mtch(ground, shape) && instT.isDefined) f(instT.get)
    else TypeMatchMismatch(ground, shape).toType
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
        val optErrorTpe = (fd.params zip argTps) collectFirst {
          case (param, argTpe) if !conformsTo(argTpe, param.tpe) =>
            ArgumentTypeMismatch(expr, param.tpe, argTpe).toType
        }
        optErrorTpe getOrElse fd.returnType
      }
    }

    expr match {
      case MissingExpr(tpe) =>
        tpe
      case Variable(id, tpe) =>
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
          StructType(id, tps)
        }

      case Enum(id, tps, _) =>
        ifGoodTypes(tps) { tps =>
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

      case Reference(expr) =>
        assert(!isRelaxed)
        ifWellTyped(expr) { tpe =>
          RefType(tpe)
        }

      /* Reference-counted boxes */

      case RcNew(expr) =>
        assert(!isRelaxed)
        ifWellTyped(expr) { tpe =>
          RcType(tpe)
        }
      case RcClone(expr) =>
        assert(!isRelaxed)
        ifWellTyped(expr) { tpe =>
          ifTypeMatch(tpe, RefType(RcType(`?T`))) { argTpe =>
            RefType(RcType(argTpe))
          }
        }
      case RcDeref(expr) =>
        assert(!isRelaxed)
        ifWellTyped(expr) { tpe =>
          RcType(tpe)
        }
    }
  }

  protected def computeType(expr: MatchExpr): Type = {
    val MatchExpr(scrutinee, cases) = expr

    def checkBinder(binder: Option[ValDef], scrutineeTpe: Type)(
        rest: => Option[TypingError]
    ): Option[TypingError] = {
      binder collect {
        case vd if !conformsTo(vd.tpe, scrutineeTpe) =>
          Some(LetTypeMismatch(vd, scrutineeTpe, vd.tpe))
      } getOrElse (rest)
    }

    def checkPattern(pat: Pattern, scrutineeTpe: Type): Option[TypingError] = {
      def mismatch =
        Some(PatternTypeMismatch(pat, scrutineeTpe))
      def checkRest(subPatterns: Seq[Pattern], subTps: Seq[Type]) =
        (subPatterns zip subTps)
          .map(t => checkPattern(t._1, t._2))
          .find(_.isDefined)
          .flatten

      pat match {
        case WildcardPattern(binder) =>
          None // wildcard can subsume any expected type

        case LiteralPattern(binder, lit) =>
          checkBinder(binder, scrutineeTpe) {
            val litTpe = ifWellTyped(lit)(identity)
            assert(!litTpe.isError)
            if (!conformsTo(litTpe, scrutineeTpe))
              mismatch
            else
              None
          }

        case StructPattern(binder, id, subPatterns) =>
          checkBinder(binder, scrutineeTpe) {
            scrutineeTpe match {
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
          checkBinder(binder, scrutineeTpe) {
            scrutineeTpe match {
              case TupleType(scrutineeTps) if scrutineeTps.size == subPatterns.size =>
                checkRest(subPatterns, scrutineeTps)
              case _ =>
                mismatch
            }
          }
      }
    }

    def caseType(cse: MatchCase, scrutineeTpe: Type): Type = {
      checkPattern(cse.pattern, scrutineeTpe).map(_.toType).getOrElse {
        ifWellTyped(cse.optGuard.getOrElse(BoolLiteral(true))) { guardTpe =>
          if (!conformsTo(guardTpe, BoolType()))
            ConditionTypeMismatch(cse, guardTpe).toType
          else
            getType(cse.rhs)
        }
      }
    }

    ifWellTyped(scrutinee) { scrutineeTpe =>
      ifGoodTypes(cases.map(cse => caseType(cse, scrutineeTpe))) { caseTps =>
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

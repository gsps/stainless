/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package rustgen
package generator

import ast.{IdentityProgramTransformer, TreeTransformer}
import ast.Trees._
import ast.Deconstructors.{Invocation, PatternBinder}

class MatchFlattening extends IdentityProgramTransformer {
  class MatchFlattener(implicit symbols: Symbols) extends TreeTransformer {
    override def transform(e: Expr): Expr = {
      type Step = (Variable, Pattern) // a binding, and the pattern it ought to be matched against
      type Stratum = Seq[Step] // steps that are matched simultaneously (using a tuple pattern)
      type Strata = Seq[Stratum] // for a given case, all the "layers" of simultaneous matching

      def stratify(scrutVar: Variable, mtch: MatchExpr): Seq[(Strata, Option[Expr], Expr)] = {
        def wildcardInStruct(binder: Option[ValDef], field: ValDef): WildcardPattern =
          WildcardPattern(Some(binder.getOrElse(ValDef.fresh(field.id.name, field.tpe, true))))

        def wildcardInTuple(binder: Option[ValDef], tpeAndIndex: (Type, Int)): WildcardPattern = {
          val (tpe, index) = tpeAndIndex
          WildcardPattern(Some(binder.getOrElse(ValDef.fresh(s"_$index", tpe, true))))
        }

        def withoutBinder(pat: Pattern): Pattern =
          pat match {
            case PatternBinder(None, _)         => pat
            case PatternBinder(Some(_), recons) => recons(None)
          }

        def unfoldSub[F](
            fields: Seq[F],
            subPats: Seq[Pattern],
            buildWildcard: (Option[ValDef], F) => WildcardPattern
        ): (Seq[Pattern], Stratum) = {
          val (newSubPatterns, restOpts) = ((fields zip subPats) map {
            case (_, subPat: WildcardPattern) =>
              (subPat, None)
            case (_, subPat: LiteralPattern[_]) =>
              (subPat, None)
            case (field, subPat) =>
              val wildcard = buildWildcard(subPat.binder, field)
              val binder = wildcard.binder.get
              (wildcard, Some(binder.toVariable -> subPat))
          }).unzip
          (newSubPatterns, restOpts.flatten)
        }

        def unfold(nested: Stratum): Strata = {
          if (nested.isEmpty) {
            Nil
          } else {
            val (shallow, nesteds): (Stratum, Strata) = (nested map {
              case (scrutV, pat @ StructPattern(_, id, subPatterns)) =>
                val fields = scrutV.tpe match {
                  case StructType(_, Seq()) => symbols.getStruct(id).fields
                  case EnumType(_, Seq())   => symbols.getEnumVariant(id).fields
                }
                val (newSubPatterns, rest) = unfoldSub(fields, subPatterns, wildcardInStruct)
                val newPat = pat.copy(subPatterns = newSubPatterns).copiedFrom(pat)
                ((scrutV, newPat), rest)

              case (scrutV, pat @ TuplePattern(_, subPatterns)) =>
                val TupleType(subScrutTps) = scrutV.tpe
                val (newSubPatterns, rest) =
                  unfoldSub(subScrutTps.zipWithIndex, subPatterns, wildcardInTuple)
                val newPat = pat.copy(subPatterns = newSubPatterns).copiedFrom(pat)
                ((scrutV, newPat), rest)

              case step =>
                (step, Seq.empty)
            }).unzip
            shallow +: unfold(nesteds.flatten)
          }
        }

        mtch.cases map { cse =>
          (unfold(Seq((scrutVar, cse.pattern))), cse.optGuard, cse.rhs)
        }
      }

      def rebuildDeepCase(
          strata: Strata,
          optGuard: Option[Expr],
          rhs: Expr
      ): MatchExpr = {
        def buildOne(stratum: Stratum, optGuard: Option[Expr], body: Expr): MatchExpr = {
          val (scrut, pattern) =
            if (stratum.size == 1) {
              stratum.head
            } else {
              val (ss, ps) = stratum.unzip
              (Tuple(ss), TuplePattern(None, ps))
            }
          val caseSuccess = MatchCase(pattern, optGuard, body)
          val caseFail = MatchCase(WildcardPattern(None), None, UnitLiteral())
          MatchExpr(scrut, Seq(caseSuccess, caseFail))
        }
        val innerMatch = buildOne(strata.last, optGuard, rhs)
        strata.init.foldRight(innerMatch) {
          case (stratum, rest) => buildOne(stratum, None, rest)
        }
      }

      e match {
        case mtch: MatchExpr =>
          val mtchAfter = super.transform(mtch).asInstanceOf[MatchExpr]
          val scrutVd = ValDef.fresh("scrutinee", mtchAfter.scrutinee.getType, true)
          val stratCases = stratify(scrutVd.toVariable, mtchAfter)
          if (stratCases.exists(_._1.size > 1)) {
            val labelSuccess = Identifier("mtch", true)
            val newMatches = stratCases
              .map {
                case (strata, optGuard, rhs) =>
                  val breakingRhs = Break(labelSuccess, rhs).copiedFrom(rhs)
                  rebuildDeepCase(strata, optGuard, breakingRhs).copiedFrom(mtch)
              }
              .foldRight[Expr](Error(mtchAfter.getType, "Match error")) {
                case (mtch, rest) => Sequence(mtch, rest).copiedFrom(mtch)
              }
            val body = Let(scrutVd, mtchAfter.scrutinee, newMatches).copiedFrom(mtch)
            LabelledBlock(labelSuccess, body).copiedFrom(mtch)
          } else {
            mtchAfter
          }
        case _ =>
          super.transform(e)
      }
    }
  }

  override def transformFunctions(
      functions: Seq[FunDef]
  )(implicit symbols: Symbols): Seq[FunDef] = {
    val matchFlattener = new MatchFlattener()
    functions.map(matchFlattener.transform)
  }
}

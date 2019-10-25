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
        def withoutBinder(pat: Pattern): Pattern =
          pat match {
            case PatternBinder(None, _)         => pat
            case PatternBinder(Some(_), recons) => recons(None)
          }

        // Unfold each tuple subpattern and adapt struct subpatterns to ensure they have a binder.
        // Struct subpatterns form the boundary; tuple- and other primitive subpatterns are kept.
        def unfoldSubSteps(
            fields: Seq[(String, Type)],
            subPats: Seq[Pattern],
            recurseOnStruct: Boolean
        ): (Seq[Pattern], Strata) = {
          (fields.zip(subPats) map {
            case ((_, subTpe), subPat: StructPattern) if recurseOnStruct =>
              unfoldStep(subTpe, subPat)
            case ((_, subTpe), subPat: TuplePattern) =>
              unfoldStep(subTpe, subPat)

            case ((subName, subTpe), subPat: StructPattern) =>
              val binder = subPat.binder.getOrElse {
                ValDef.fresh(subName, subTpe, true)
              }
              val wildcard = WildcardPattern(Some(binder))
              (wildcard, Seq(binder.toVariable -> subPat))

            case (_, subPat) =>
              (subPat, Seq())
          }).unzip
        }

        // Unfold a single step as deeply as possible by recursing on tuple patterns.
        def unfoldStep(scrutTpe: Type, pat: Pattern): (Pattern, Stratum) = {
          pat match {
            case pat @ StructPattern(_, id, subPats) =>
              val fields = scrutTpe match {
                case StructType(_, Seq()) => symbols.getStruct(id).fields
                case EnumType(_, Seq())   => symbols.getEnumVariant(id).fields
              }
              val fieldInfos = fields.map(field => field.id.name -> field.tpe)
              val (newSubPatterns, rests) =
                unfoldSubSteps(fieldInfos, subPats, recurseOnStruct = false)
              val newPat = pat.copy(subPatterns = newSubPatterns).copiedFrom(pat)
              (newPat, rests.flatten)

            case pat @ TuplePattern(_, subPats) =>
              val TupleType(subScrutTps) = scrutTpe
              val fieldInfos = subScrutTps.zipWithIndex.map { case (tpe, i) => s"_$i" -> tpe }
              val (newSubPatterns, rests) =
                unfoldSubSteps(fieldInfos, subPats, recurseOnStruct = true)
              val newPat = pat.copy(subPatterns = newSubPatterns).copiedFrom(pat)
              (newPat, rests.flatten)

            case _ =>
              (pat, Seq())
          }
        }

        // Unfold a nested stratum into many shallow ones.
        def unfold(nested: Stratum): Strata = {
          if (nested.isEmpty) {
            Nil
          } else {
            val (shallow, nesteds): (Stratum, Strata) = (nested map {
              case (scrutV, pat) =>
                val (newPat, rest) = unfoldStep(scrutV.tpe, pat)
                ((scrutV, newPat), rest)
            }).unzip
            shallow +: unfold(nesteds.flatten)
          }
        }

        mtch.cases map { cse =>
          (unfold(Seq((scrutVar, cse.pattern))), cse.optGuard, cse.rhs)
        }
      }

      def rebuildStratifiedCase(
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

      /*
      def dumpStratifiedCases(stratCases: Seq[(Strata, Option[Expr], Expr)]): Unit = {
        println("---")
        stratCases.foreach {
          case (strata, optGuard, rhs) =>
            println(s"* $optGuard / $rhs / strata:")
            strata.foreach { stratum =>
              println(
                s"  - ${stratum.map { case (v, pat) => s"${v.show()} @ ${pat.show()}" }.mkString(", ")}"
              )
            }
        }
      }
       */

      e match {
        case mtch: MatchExpr =>
          val mtchAfter = super.transform(mtch).asInstanceOf[MatchExpr]
          val scrutVd = ValDef.fresh("scrutinee", mtchAfter.scrutinee.getType, true)
          val stratCases = stratify(scrutVd.toVariable, mtchAfter)
          if (stratCases.exists(_._1.size > 1)) {
            val resultTpe = mtch.getType
            val labelSuccess = ValDef.fresh("mtch", resultTpe, alwaysShowUniqueID = true)
            val newMatches = stratCases
              .map {
                case (strata, optGuard, rhs) =>
                  val breakingRhs = Break(labelSuccess, UnitType(), rhs).copiedFrom(rhs)
                  rebuildStratifiedCase(strata, optGuard, breakingRhs).copiedFrom(mtch)
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

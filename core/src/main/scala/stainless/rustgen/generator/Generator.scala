/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package rustgen
package generator

import inox.utils.Bijection

import scala.reflect.ClassTag

object DebugSectionRustgenGenerator extends inox.DebugSection("rustgen-generator")


class Generator()(implicit context: inox.Context, symbols: stainless.trees.Symbols) {
  import stainless.{trees => st}
  import generator.{rust => rt}

  protected implicit def printerOpts: st.PrinterOptions =
    st.PrinterOptions.fromContext(context)

  import scala.language.implicitConversions

  implicit def id2id(id: stainless.Identifier): rt.Identifier =
    new rt.Identifier(id)

  val enumMap: Bijection[rt.Identifier, rt.EnumDef] = Bijection()
  val functionMap: Bijection[rt.Identifier, rt.FunDef] = Bijection()

  def toProgram(implicit symbols: stainless.trees.Symbols): rt.Program = {
    val enums = enumMap.toSeq sortWith { case ((id1, _), (id2, _)) =>
      val pos1 = symbols.getFunction(id1).getPos
      val pos2 = symbols.getFunction(id2).getPos
      (pos1 compare pos2) < 0
    } map { _._2 }
    val functions = functionMap.toSeq sortWith { case ((id1, _), (id2, _)) =>
      val pos1 = symbols.getFunction(id1).getPos
      val pos2 = symbols.getFunction(id2).getPos
      (pos1 compare pos2) < 0
    } map { _._2 }
    val module = rt.ModuleDef(
      rt.Identifier("stainlessExport"), None, enums, functions)
    rt.Program(Seq(module))
  }

  def translate(defn: st.Definition): rt.Tree = {
    defn match {
      case sort: st.ADTSort => translate(sort)
      case fd: st.FunDef => translate(fd)
    }
  }

  def translate(sort: st.ADTSort): rt.EnumDef = {
    assert(sort.tparams.isEmpty)
    val enumId: rt.Identifier = sort.id
    enumMap.cachedB(enumId){
      val variants = sort.constructors map { cons =>
        val fields = cons.fields.map(translate)
        rt.EnumVariant(cons.id, enumId, fields)
      }
      rt.EnumDef(sort.id, variants)
    }
  }

  def translate(fd: st.FunDef): rt.FunDef = {
    assert(fd.tparams.isEmpty)
    val funId: rt.Identifier = fd.id
    functionMap.cachedB(funId){
      rt.FunDef(funId,
        fd.params.map(translate),
        translate(fd.returnType),
        translate(fd.fullBody))
    }
  }

  def translate(vd: st.ValDef): rt.ValDef = {
    rt.ValDef(vd.id, translate(vd.tpe))
  }

  def translate(tpe: st.Type): rt.Type = {
    tpe match {
      case st.UnitType()       => rt.UnitType
      case st.BooleanType()    => rt.BoolType
      case st.Int32Type()      => rt.I32Type
      case st.StringType()     => rt.StrType
      case st.TupleType(tps)   =>
        rt.TupleType(tps.map(translate))
      case st.ADTType(id, tps) =>
        assert(tps.isEmpty)
        rt.stdRc.RcType(rt.EnumType(id, Seq.empty))
    }
  }

  def refIfNonADT[S <: st.Expr](f: S => rt.Expr)(expr: S): rt.Expr = {
    val rtExpr = f(expr)
    // NOTE: Can't currently do this test on the translated type, since
    //       we don't have reliable information on the resulting rust type
    //       without the context we're translating in (for instance, in matches,
    //       where we implicitly bind references of the scrutinee's parts).
    expr.getType match {
      case st.ADTType(_, _) => rtExpr
      case _ => rt.Reference(rtExpr)
    }
  }

  def cloneIfADT[S <: st.Expr](f: S => rt.Expr)(expr: S): rt.Expr = {
    val rtExpr = f(expr)
    expr.getType match {
      case st.ADTType(_, _) => rt.MethodInvocation(rt.stdRc.klone, rtExpr, Seq.empty)
      case _ => rtExpr
    }
  }

  def translate(expr: st.Expr): rt.Expr = {
    expr match {
      case st.Variable(id, _, _) => rt.Variable(id)

      case expr: st.Literal[_] => translate(expr)

      case st.ADT(id, tps, args) =>
        assert(tps.isEmpty)
        val value = rt.Enum(id, Seq.empty, args.map(translate))
        rt.FunctionInvocation(rt.stdRc.neu, Seq(value))
      case st.Tuple(exprs) =>
        rt.Tuple(exprs.map(translate))

      case st.Let(vd, value, body) =>
        rt.Let(translate(vd), translate(value), translate(body))

      case expr: st.MatchExpr =>
        translate(expr)

      case st.FunctionInvocation(id, tps, args) =>
        assert(tps.isEmpty)
        rt.FunctionInvocation(id, args.map(cloneIfADT(translate)))

      case st.UMinus(expr) =>
        rt.MethodInvocation(rt.stdOps.neg, translate(expr), Seq.empty)
      case st.Plus(lhs, rhs) =>
        rt.MethodInvocation(rt.stdOps.add, translate(lhs), Seq(translate(rhs)))
      case st.Minus(lhs, rhs) =>
        rt.MethodInvocation(rt.stdOps.sub, translate(lhs), Seq(translate(rhs)))
      case st.Times(lhs, rhs) =>
        rt.MethodInvocation(rt.stdOps.mul, translate(lhs), Seq(translate(rhs)))
      case st.Division(lhs, rhs) =>
        rt.MethodInvocation(rt.stdOps.div, translate(lhs), Seq(translate(rhs)))
      case st.Modulo(lhs, rhs) =>
        ???
      case st.Remainder(lhs, rhs) =>
        ???

      case st.BVNot(expr) =>
        rt.MethodInvocation(rt.stdOps.not, translate(expr), Seq.empty)
      // ...

      case st.LessThan(lhs, rhs) =>
        val arg = refIfNonADT[st.Expr](translate)(rhs)
        rt.MethodInvocation(rt.stdCmp.lt, translate(lhs), Seq(arg))
      case st.LessEquals(lhs, rhs) =>
        val arg = refIfNonADT[st.Expr](translate)(rhs)
        rt.MethodInvocation(rt.stdCmp.le, translate(lhs), Seq(arg))
      case st.GreaterThan(lhs, rhs) =>
        val arg = refIfNonADT[st.Expr](translate)(rhs)
        rt.MethodInvocation(rt.stdCmp.gt, translate(lhs), Seq(arg))
      case st.GreaterEquals(lhs, rhs) =>
        val arg = refIfNonADT[st.Expr](translate)(rhs)
        rt.MethodInvocation(rt.stdCmp.ge, translate(lhs), Seq(arg))
      case st.Equals(lhs, rhs) =>
        val arg = refIfNonADT[st.Expr](translate)(rhs)
        rt.MethodInvocation(rt.stdCmp.eq, translate(lhs), Seq(arg))

      case st.IfExpr(cond, thenn, elze) =>
        rt.IfExpr(translate(cond), translate(thenn), translate(elze))

      case st.Annotated(body, _) => translate(body)

      case _ =>
        throw new IllegalArgumentException(s"Unsupported expression '$expr'")
    }
  }

  def translate[T](lit: st.Literal[T]): rt.Literal[T] = {
    lit match {
      case st.UnitLiteral() =>
        rt.UnitLiteral()
      case st.Int32Literal(value) =>
        rt.IntLiteral(value, rt.I32Type)
          .asInstanceOf[rt.Literal[T]]  // unapply doesn't constrain T here
      case st.StringLiteral(value) =>
        rt.StrLiteral(value)
    }
  }

  private def translate(expr: st.MatchExpr): rt.MatchExpr = {
    def translatePattern(pat: st.Pattern): rt.Pattern = {
      val rtBinder = pat.binder.map(translate)
      pat match {
        case st.WildcardPattern(_) =>
          rt.WildcardPattern(rtBinder)
        case st.LiteralPattern(_, lit) =>
          rt.LiteralPattern(rtBinder, translate(lit))
        case st.ADTPattern(_, id, tps, subPatterns) =>
          assert(tps.isEmpty)
          rt.StructPattern(rtBinder, id, subPatterns.map(translatePattern))
        case st.TuplePattern(_, subPatterns) =>
          rt.TuplePattern(rtBinder, subPatterns.map(translatePattern))
      }
    }

    def translateCase(cse: st.MatchCase): rt.MatchCase = {
      val st.MatchCase(pattern, optGuard, rhs) = cse
      rt.MatchCase(translatePattern(pattern), optGuard.map(translate), translate(rhs))
    }

    val rtScrutinee = {
      val translated = translate(expr.scrutinee)
      expr.scrutinee.getType match {
        case st.ADTType(_, _) =>
          rt.MethodInvocation(rt.stdRc.as_ref, translated, Seq.empty)
        case _ => translated
      }
    }
    rt.MatchExpr(rtScrutinee, expr.cases.map(translateCase))
  }
}

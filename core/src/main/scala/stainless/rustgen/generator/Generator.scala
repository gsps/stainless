/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package rustgen
package generator

import inox.utils.Bijection

object DebugSectionRustgenGenerator extends inox.DebugSection("rustgen-generator")


class Generator()(implicit context: inox.Context, symbols: stainless.trees.Symbols) {
  import stainless.{trees => st}
  import generator.{rust => rt}

  protected implicit def printerOpts: st.PrinterOptions =
    st.PrinterOptions.fromContext(context)

  import scala.language.implicitConversions

  implicit def id2id(id: stainless.Identifier): rt.Identifier =
    new rt.Identifier(id)

  val enumMap: Bijection[rt.Identifier, rt.Enum] = Bijection()
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

  def translate(sort: st.ADTSort): rt.Enum = {
    assert(sort.tparams.isEmpty)
    val enumId: rt.Identifier = sort.id
    enumMap.cachedB(enumId){
      val variants = sort.constructors map { cons =>
        val fields = cons.fields.map(translate)
        rt.EnumVariant(cons.id, enumId, fields)
      }
      rt.Enum(sort.id, variants)
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
      case st.TupleType(tps)   => rt.RefType(rt.TupleType(tps.map(translate)))
      case st.ADTType(id, tps) =>
        assert(tps.isEmpty)
        val enumId: rt.Identifier = id
        rt.RefType(rt.EnumType(enumMap.toB(enumId)))
    }
  }

  def ensureRef[S <: st.Expr](expr: S)(f: S => rt.Expr): rt.Expr = {
    val rtexpr = f(expr)
    translate(expr.getType) match {
      case rt.RefType(_) => rtexpr
      case _ => rt.Reference(rtexpr)
    }
  }

  def translate(expr: st.Expr): rt.Expr = {
    expr match {
      case st.Variable(id, _, _) => rt.Variable(id)

      case st.UnitLiteral() => rt.UnitLiteral()
      case st.Int32Literal(value) => rt.IntLiteral(value, rt.I32Type)
      case st.StringLiteral(value) => rt.StrLiteral(value)

      case st.Let(vd, value, body) =>
        rt.Let(translate(vd), translate(value), translate(body))
      case st.FunctionInvocation(id, tps, args) =>
        assert(tps.isEmpty)
        rt.FunctionInvocation(id, args.map(translate))

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
          rt.MethodInvocation(rt.stdCmp.lt, translate(lhs), Seq(ensureRef(rhs)(translate)))
        case st.LessEquals(lhs, rhs) =>
          rt.MethodInvocation(rt.stdCmp.le, translate(lhs), Seq(ensureRef(rhs)(translate)))
        case st.GreaterThan(lhs, rhs) =>
          rt.MethodInvocation(rt.stdCmp.gt, translate(lhs), Seq(ensureRef(rhs)(translate)))
        case st.GreaterEquals(lhs, rhs) =>
          rt.MethodInvocation(rt.stdCmp.ge, translate(lhs), Seq(ensureRef(rhs)(translate)))
        case st.Equals(lhs, rhs) =>
          rt.MethodInvocation(rt.stdCmp.eq, translate(lhs), Seq(ensureRef(rhs)(translate)))

        case st.IfExpr(cond, thenn, elze) =>
          rt.IfExpr(translate(cond), translate(thenn), translate(elze))

        case st.Annotated(body, _) => translate(body)

        case _ =>
          throw new IllegalArgumentException(s"Unsupported expression '$expr'")
    }
  }
}

/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package rustgen
package generator

import scala.collection.mutable.{Map => MutableMap}

object DebugSectionRustgenGenerator extends inox.DebugSection("rustgen-generator")


class Generator(implicit context: inox.Context) {
  import stainless.{trees => st}
  import generator.{rust => rt}

  protected implicit def printerOpts: st.PrinterOptions =
    st.PrinterOptions.fromContext(context)

  import scala.language.implicitConversions

  implicit def id2id(id: stainless.Identifier): rt.Identifier =
    new rt.Identifier(id)

  val enumMap: MutableMap[rt.Identifier, rt.Enum] = MutableMap.empty

  def translate(defn: st.Definition): rt.Tree = {
    defn match {
      case sort: st.ADTSort => translate(sort)
      case fd: st.FunDef => translate(fd)
    }
  }

  def translate(sort: st.ADTSort): rt.Enum = {
    assert(sort.tparams.isEmpty)
    val enumId: rt.Identifier = sort.id
    assert(!enumMap.isDefinedAt(enumId))
    val variants = sort.constructors map { cons =>
      val fields = cons.fields.map(translate)
      rt.EnumVariant(cons.id, enumId, fields)
    }
    rt.Enum(sort.id, variants)
  }

  def translate(fd: st.FunDef): rt.FunDef = {
    assert(fd.tparams.isEmpty)
    rt.FunDef(fd.id,
      fd.params.map(translate),
      translate(fd.returnType),
      translate(fd.fullBody))
  }

  def translate(vd: st.ValDef): rt.ValDef = {
    rt.ValDef(vd.id, translate(vd.tpe))
  }

  def translate(tpe: st.Type): rt.Type = {
    tpe match {
      case st.BooleanType()    => rt.BoolType
      case st.Int32Type()      => rt.I32Type
      case st.StringType()     => rt.StrType
      case st.TupleType(tps)   => rt.TupleType(tps.map(translate))
      case st.ADTType(id, tps) =>
        assert(tps.isEmpty)
        val enumId: rt.Identifier = id
        assert(enumMap.isDefinedAt(enumId))
        rt.EnumType(enumMap(enumId))
    }
  }

  def translate(expr: st.Expr): rt.Expr = {
    expr match {
      case st.Variable(id, _, _) => rt.Variable(id)
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
          rt.MethodInvocation(rt.stdCmp.lt, translate(lhs), Seq(translate(rhs)))
        case st.LessEquals(lhs, rhs) =>
          rt.MethodInvocation(rt.stdCmp.le, translate(lhs), Seq(translate(rhs)))
        case st.GreaterThan(lhs, rhs) =>
          rt.MethodInvocation(rt.stdCmp.gt, translate(lhs), Seq(translate(rhs)))
        case st.GreaterEquals(lhs, rhs) =>
          rt.MethodInvocation(rt.stdCmp.ge, translate(lhs), Seq(translate(rhs)))
        case st.Equals(lhs, rhs) =>
          rt.MethodInvocation(rt.stdCmp.eq, translate(lhs), Seq(translate(rhs)))

        case st.IfExpr(cond, thenn, elze) =>
          rt.IfExpr(translate(cond), translate(thenn), translate(elze))

        case st.Int32Literal(value) =>
          rt.IntLiteral(value, rt.I32Type)

        case _ =>
          throw new IllegalArgumentException(s"Unsupported expression '$expr'")
    }
  }
}

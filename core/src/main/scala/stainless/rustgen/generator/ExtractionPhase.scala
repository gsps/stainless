/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package rustgen
package generator

import collection.mutable.{Map => MutableMap}

class ExtractionPhase(_inoxSymbols: stainless.trees.Symbols) {
  import stainless.{trees => st}
  import ast.{Trees => rt}

  implicit val inoxSymbols: st.Symbols = _inoxSymbols

  /* State */

  val structMap: MutableMap[rt.Identifier, rt.StructDef] = MutableMap()
  val enumMap: MutableMap[rt.Identifier, rt.EnumDef] = MutableMap()
  val functionMap: MutableMap[rt.Identifier, rt.FunDef] = MutableMap()

  def initWithLibrary() = {
    RustLibrary.structs.foreach(item => structMap += item.id -> item)
    RustLibrary.enums.foreach(item => enumMap += item.id -> item)
    RustLibrary.functions.foreach(item => functionMap += item.id -> item)
  }
  initWithLibrary()

  /* Entrypoint */

  def apply(sorts: Seq[st.ADTSort], functions: Seq[st.FunDef]): rt.Program = {
    sorts.foreach(translate)
    functions.foreach(translate)
    val rtSymbols =
      rt.Symbols(structMap.toMap, enumMap.toMap, functionMap.toMap, strictTyping = false)
    rt.Program(rtSymbols)
  }

  /* Translation of top-level constructs */

  protected def translate(sort: st.ADTSort): rt.EnumDef = {
    assert(sort.tparams.isEmpty)
    val enumId: rt.Identifier = sort.id
    enumMap.getOrElseUpdate(
      enumId, {
        val variants = sort.constructors map { cons =>
          val fields = cons.fields.map(translate)
          rt.EnumVariant(cons.id, enumId, fields).copiedFrom(cons)
        }
        rt.EnumDef(sort.id, variants).copiedFrom(sort)
      }
    )
  }

  protected def translate(fd: st.FunDef): rt.FunDef = {
    assert(fd.tparams.isEmpty)
    val funId: rt.Identifier = fd.id
    functionMap.getOrElseUpdate(
      funId, {
        val flags =
          if (fd.flags.exists(f => f == st.Synthetic || f == st.Library)) Seq(rt.Library)
          else Seq()
        rt.FunDef(
            funId,
            fd.params.map(translate),
            translate(fd.returnType),
            translate(fd.fullBody),
            flags
          )
          .copiedFrom(fd)
      }
    )
  }

  /* Expression-level translation */

  protected def translate(vd: st.ValDef): rt.ValDef = {
    rt.ValDef(vd.id, translate(vd.tpe)).copiedFrom(vd)
  }

  protected def translate(tpe: st.Type): rt.Type = {
    (tpe match {
      case st.UnitType()    => rt.UnitType()
      case st.BooleanType() => rt.BoolType()
      case st.Int32Type()   => rt.I32Type()
      case st.StringType()  => rt.StrType()
      case st.TupleType(tps) =>
        rt.TupleType(tps.map(translate))
      case st.ADTType(id, tps) =>
        assert(tps.isEmpty)
        rt.EnumType(id, Seq.empty)
    }).copiedFrom(tpe)
  }

  object StainlessError {
    // FIXME: Find a more robust way of compiling away stainless' "error"?
    def unapply(expr: st.Expr): Option[(st.Type, String)] = {
      expr match {
        case st.Error(tpe, reason) =>
          Some((tpe, reason))
        case st.Let(_, st.Error(_, reason), st.Annotated(st.ADTSelector(adt, selector), flags))
            if flags.contains(st.Unchecked) && selector.name == "value" =>
          Some((expr.getType, reason))
        case _ =>
          None
      }
    }
  }

  protected def translate(expr: st.Expr): rt.Expr = {
    (expr match {
      case StainlessError(tpe, reason) =>
        rt.Error(translate(tpe), reason)

      case st.Variable(id, tpe, _) =>
        rt.Variable(id, translate(tpe), Seq.empty)

      case expr: st.Literal[_] =>
        translateLiteral(expr)

      case st.ADT(id, tps, args) =>
        assert(tps.isEmpty)
        rt.Enum(id, Seq.empty, args.map(translate))
      case st.Tuple(exprs) =>
        rt.Tuple(exprs.map(translate))

      case st.Let(vd, value, body) =>
        rt.Let(translate(vd), translate(value), translate(body))

      case expr: st.MatchExpr =>
        translateMatch(expr)

      case st.FunctionInvocation(id, tps, args) =>
        assert(tps.isEmpty)
        rt.FunctionInvocation(id, args.map(translate))

      case st.UMinus(expr) =>
        rt.MethodInvocation(RustLibrary.ops.neg, translate(expr), Seq.empty)
      case st.Plus(lhs, rhs) =>
        rt.MethodInvocation(RustLibrary.ops.add, translate(lhs), Seq(translate(rhs)))
      case st.Minus(lhs, rhs) =>
        rt.MethodInvocation(RustLibrary.ops.sub, translate(lhs), Seq(translate(rhs)))
      case st.Times(lhs, rhs) =>
        rt.MethodInvocation(RustLibrary.ops.mul, translate(lhs), Seq(translate(rhs)))
      case st.Division(lhs, rhs) =>
        rt.MethodInvocation(RustLibrary.ops.div, translate(lhs), Seq(translate(rhs)))
      case st.Modulo(lhs, rhs) =>
        ???
      case st.Remainder(lhs, rhs) =>
        ???

      case st.BVNot(expr) =>
        rt.MethodInvocation(RustLibrary.ops.not, translate(expr), Seq.empty)
      // ...

      case st.LessThan(lhs, rhs) =>
        rt.MethodInvocation(RustLibrary.cmp.lt, translate(lhs), Seq(translate(rhs)))
      case st.LessEquals(lhs, rhs) =>
        rt.MethodInvocation(RustLibrary.cmp.le, translate(lhs), Seq(translate(rhs)))
      case st.GreaterThan(lhs, rhs) =>
        rt.MethodInvocation(RustLibrary.cmp.gt, translate(lhs), Seq(translate(rhs)))
      case st.GreaterEquals(lhs, rhs) =>
        rt.MethodInvocation(RustLibrary.cmp.ge, translate(lhs), Seq(translate(rhs)))
      case st.Equals(lhs, rhs) =>
        rt.MethodInvocation(RustLibrary.cmp.eq, translate(lhs), Seq(translate(rhs)))

      case st.IfExpr(cond, thenn, elze) =>
        rt.IfExpr(translate(cond), translate(thenn), translate(elze))

      case st.Annotated(body, _) => translate(body)

      case _ =>
        throw new NotImplementedError(s"Unsupported expression '$expr' (${expr.getClass})")
    }).copiedFrom(expr)
  }

  protected def translateLiteral[T](lit: st.Literal[T]): rt.Literal[T] = {
    lit match {
      case st.UnitLiteral() =>
        rt.UnitLiteral()
      case st.BooleanLiteral(value) =>
        rt.BoolLiteral(value)
      case st.Int32Literal(value) =>
        rt.IntLiteral(value, rt.I32Type())
          .asInstanceOf[rt.Literal[T]] // unapply doesn't constrain T here
      case st.StringLiteral(value) =>
        rt.StrLiteral(value)
    }
  }

  protected def translateMatch(expr: st.MatchExpr): rt.MatchExpr = {
    def translatePattern(pat: st.Pattern): rt.Pattern = {
      val rtBinder = pat.binder.map(translate)
      (pat match {
        case st.WildcardPattern(_) =>
          rt.WildcardPattern(rtBinder)
        case st.LiteralPattern(_, lit) =>
          rt.LiteralPattern(rtBinder, translateLiteral(lit))
        case st.ADTPattern(_, id, tps, subPatterns) =>
          assert(tps.isEmpty)
          rt.StructPattern(rtBinder, id, subPatterns.map(translatePattern))
        case st.TuplePattern(_, subPatterns) =>
          rt.TuplePattern(rtBinder, subPatterns.map(translatePattern))
      }).copiedFrom(pat)
    }

    def translateCase(cse: st.MatchCase): rt.MatchCase = {
      val st.MatchCase(pattern, optGuard, rhs) = cse
      rt.MatchCase(translatePattern(pattern), optGuard.map(translate), translate(rhs))
        .copiedFrom(cse)
    }

    rt.MatchExpr(translate(expr.scrutinee), expr.cases.map(translateCase))
  }
}

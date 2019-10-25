/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package rustgen
package ast

import Trees._

// TODO: Add transformations for `Flag`s
trait Transformer {
  type Env

  // Produce the environment for the non-expression parts of a given expression
  def nonExpressionEnv(expr: Expr, env: Env): Env = env

  // Produce the environments for each of the expression parts of a given expression
  def expressionEnvs(expr: Expr, subExprs: Seq[Expr], env: Env): Seq[Env] =
    Seq.fill(subExprs.length)(env)

  def transform(id: Identifier, env: Env): Identifier = id

  def transform(id: Identifier, tpe: Type, env: Env): (Identifier, Type) = {
    (transform(id, env), transform(tpe, env))
  }

  def transform(vd: ValDef, env: Env): ValDef = {
    val ValDef(id, tpe, flags) = vd
    val (newId, newTpe) = transform(id, tpe, env)

    // var changed = false
    // val newFlags = for (f <- flags) yield {
    //   val newFlag = transform(f, env)
    //   if (f ne newFlag) changed = true
    //   newFlag
    // }

    if ((id ne newId) || (tpe ne newTpe) /*|| changed*/ ) {
      // ValDef(newId, newTpe, newFlags).copiedFrom(vd)
      ValDef(newId, newTpe, flags).copiedFrom(vd)
    } else {
      vd
    }
  }

  // def transform(tpd: TypeParameterDef, env: Env): TypeParameterDef = {
  //   val newTp = transform(tpd.tp, env)
  //
  //   if ((tpd.tp ne newTp) || (s ne t)) {
  //     TypeParameterDef(newTp.asInstanceOf[t.TypeParameter])
  //   } else {
  //     tpd.asInstanceOf[t.TypeParameterDef]
  //   }
  // }

  def transform(e: Expr, env: Env): Expr = {
    val (ids, vs, es, tps /*, flags*/, builder) = TreeDeconstructor.deconstruct(e)

    val nonExprEnv = nonExpressionEnv(e, env)
    var changed = false

    val newIds = for (id <- ids) yield {
      val newId = transform(id, nonExprEnv)
      if (id ne newId) changed = true
      newId
    }

    val newVs = for (v <- vs) yield {
      val vd = v.toVal
      val newVd = transform(vd, nonExprEnv)
      if (vd ne newVd) changed = true
      newVd.toVariable
    }

    val newEs = for ((e, eEnv) <- es.zip(expressionEnvs(e, es, env))) yield {
      val newE = transform(e, eEnv)
      if (e ne newE) changed = true
      newE
    }

    val newTps = for (tp <- tps) yield {
      val newTp = transform(tp, nonExprEnv)
      if (tp ne newTp) changed = true
      newTp
    }

    // val newFlags = for (flag <- flags) yield {
    //   val newFlag = transform(flag, nonExprEnv)
    //   if (flag ne newFlag) changed = true
    //   newFlag
    // }

    if (changed) {
      builder(newIds, newVs, newEs, newTps /*, newFlags*/ ).copiedFrom(e)
    } else {
      e
    }
  }

  def transform(tpe: Type, env: Env): Type = {
    val (ids, vs, es, tps /*, flags*/, builder) = TreeDeconstructor.deconstruct(tpe)

    var changed = false

    val newIds = for (id <- ids) yield {
      val newId = transform(id, env)
      if (id ne newId) changed = true
      newId
    }

    val newVs = for (v <- vs) yield {
      val vd = v.toVal
      val newVd = transform(vd, env)
      if (vd ne newVd) changed = true
      newVd.toVariable
    }

    val newEs = for (e <- es) yield {
      val newE = transform(e, env)
      if (e ne newE) changed = true
      newE
    }

    val newTps = for (tp <- tps) yield {
      val newTp = transform(tp, env)
      if (tp ne newTp) changed = true
      newTp
    }

    // val newFlags = for (f <- flags) yield {
    //   val newFlag = transform(f, env)
    //   if (f ne newFlag) changed = true
    //   newFlag
    // }

    if (changed) {
      builder(newIds, newVs, newEs, newTps /*, newFlags*/ ).copiedFrom(tpe)
    } else {
      tpe
    }
  }

  // def transform(flag: Flag, env: Env): Flag = {
  //   val (ids, es, tps, builder) = TreeDeconstructor.deconstruct(flag)
  //
  //   var changed = false
  //   val newIds = for (id <- ids) yield {
  //     val newId = transform(id, env)
  //     if (id ne newId) changed = true
  //     newId
  //   }
  //
  //   val newEs = for (e <- es) yield {
  //     val newE = transform(e, env)
  //     if (e ne newE) changed = true
  //     newE
  //   }
  //
  //   val newTps = for (tp <- tps) yield {
  //     val newTp = transform(tp, env)
  //     if (tp ne newTp) changed = true
  //     newTp
  //   }
  //
  //   if (changed) {
  //     builder(newIds, newEs, newTps)
  //   } else {
  //     flag
  //   }
  // }
}

trait DefinitionTransformer extends Transformer {
  def initEnv: Env

  def transform(fd: FunDef): FunDef = {
    val env = initEnv

    FunDef(
      transform(fd.id, env),
      // fd.tparams map (transform(_, env)),
      fd.params map (transform(_, env)),
      transform(fd.returnType, env),
      transform(fd.body, env),
      fd.flags // fd.flags map (transform(_, env))
    ).copiedFrom(fd)
  }

  def transform(struct: StructDef): StructDef = {
    val env = initEnv

    StructDef(
      transform(struct.id, env),
      // struct.tparams map (transform(_, env)),
      struct.fields map (transform(_, env))
      // struct.flags map (transform(_, env))
    ).copiedFrom(struct)
  }

  def transform(enm: EnumDef): EnumDef = {
    val env = initEnv

    EnumDef(
      transform(enm.id, env),
      // enm.tparams map (transform(_, env)),
      enm.variants map { vari =>
        new EnumVariant(
          transform(vari.id, env),
          transform(vari.enm, env),
          vari.fields map (transform(_, env))
        ).copiedFrom(vari)
      }
      // enm.flags map (transform(_, env))
    ).copiedFrom(enm)
  }
}

trait TreeTransformer extends DefinitionTransformer {
  override final type Env = Unit
  override final val initEnv: Unit = ()

  def transform(id: Identifier): Identifier = super.transform(id, ())
  override final def transform(id: Identifier, env: Env): Identifier = transform(id)

  def transform(id: Identifier, tpe: Type): (Identifier, Type) = super.transform(id, tpe, ())
  override final def transform(id: Identifier, tpe: Type, env: Env): (Identifier, Type) =
    transform(id, tpe)

  def transform(vd: ValDef): ValDef = super.transform(vd, ())
  override final def transform(vd: ValDef, env: Env): ValDef = transform(vd)

  // def transform(tpd: TypeParameterDef): TypeParameterDef = super.transform(tpd, ())
  // override final def transform(tpd: TypeParameterDef, env: Env): TypeParameterDef = transform(tpd)

  def transform(e: Expr): Expr = super.transform(e, ())
  override final def transform(e: Expr, env: Env): Expr = transform(e)

  def transform(tpe: Type): Type = super.transform(tpe, ())
  override final def transform(tpe: Type, env: Env): Type = transform(tpe)

  // def transform(flag: Flag): Flag = super.transform(flag, ())
  // override final def transform(flag: Flag, env: Env): Flag = transform(flag)

  protected trait TreeTransformerComposition extends TreeTransformer {
    protected val t1: TreeTransformer
    protected val t2: TreeTransformer

    override final def transform(id: Identifier): Identifier = t2.transform(t1.transform(id))

    override final def transform(id: Identifier, tpe: Type): (Identifier, Type) = {
      val (id1, tp1) = t1.transform(id, tpe)
      t2.transform(id1, tp1)
    }

    override final def transform(vd: ValDef): ValDef = t2.transform(t1.transform(vd))
    override final def transform(e: Expr): Expr = t2.transform(t1.transform(e))
    override final def transform(tpe: Type): Type = t2.transform(t1.transform(tpe))
    // override final def transform(flag: Flag): Flag = t2.transform(t1.transform(flag))

    override final def transform(fd: FunDef): FunDef = t2.transform(t1.transform(fd))
    override final def transform(sort: StructDef): StructDef = t2.transform(t1.transform(sort))
    override final def transform(sort: EnumDef): EnumDef = t2.transform(t1.transform(sort))
  }

  def compose(that: TreeTransformer): TreeTransformer = that andThen this

  def andThen(that: TreeTransformer): TreeTransformer = new TreeTransformerComposition {
    val t1: TreeTransformer.this.type = TreeTransformer.this
    val t2: that.type = that
  }
}

trait ProgramTransformer {
  def transform(program: Program): Program
}

trait SimpleProgramTransformer extends ProgramTransformer {
  def transform(program: Program): Program = {
    implicit val oldSyms: Symbols = program.symbols
    val newSyms = Symbols(
      transformStructs(oldSyms.structs.values.toSeq),
      transformEnums(oldSyms.enums.values.toSeq),
      transformFunctions(oldSyms.functions.values.toSeq),
      oldSyms.strictTyping
    )
    Program(newSyms)
  }

  def transformStructs(structs: Seq[StructDef])(implicit symbols: Symbols): Seq[StructDef]
  def transformEnums(enums: Seq[EnumDef])(implicit symbols: Symbols): Seq[EnumDef]
  def transformFunctions(functions: Seq[FunDef])(implicit symbols: Symbols): Seq[FunDef]
}

trait IdentityProgramTransformer extends SimpleProgramTransformer {
  def transformStructs(structs: Seq[StructDef])(implicit symbols: Symbols) = structs
  def transformEnums(enums: Seq[EnumDef])(implicit symbols: Symbols) = enums
  def transformFunctions(functions: Seq[FunDef])(implicit symbols: Symbols) = functions
}

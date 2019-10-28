/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package rustgen
package generator

// Rust standard symbols
object RustLibrary {
  import ast.Trees._
  import ast.DSL._
  import scala.language.postfixOps

  /* Well-known names (referred to during lowering) */

  // object panic {
  //   val `panic!` = funName("panic!")
  // }

  object ops {
    val add = opName("add")
    val bitAnd = opName("bitAnd")
    val bitOr = opName("bitOr")
    val bitXor = opName("bitXor")
    val div = opName("div")
    val mul = opName("mul")
    val neg = opName("neg")
    val not = opName("not")
    val rem = opName("rem")
    val shl = opName("shl")
    val sub = opName("sub")
  }

  object cmp {
    val lt = opName("lt")
    val le = opName("le")
    val gt = opName("gt")
    val ge = opName("ge")
    val eq = opName("eq")
    // val ne = opName("ne") // unused!
  }

  // object rc {
  //   val Rc = structName("Rc")
  //   val neu = funName("Rc::new")
  //   val klone = funName("clone")
  //   val as_ref = funName("as_ref")
  // }

  /* Definitions */

  val structs: Seq[StructDef] = Seq(
    // mkStructDef(rc.Rc, Seq("value" :: NoType))
  )

  val enums: Seq[EnumDef] = Seq()

  val functions: Seq[FunDef] = {
    // val Rc = ST(rc.Rc)()
    Seq(
      // mkFunDef(panic.`panic!`, Seq("msg" :: StrType()), NoType),

      binOpDef(ops.add, "+", I32Type(), I32Type(), I32Type()),
      binOpDef(ops.bitAnd, "&", I32Type(), I32Type(), I32Type()),
      binOpDef(ops.bitOr, "|", I32Type(), I32Type(), I32Type()),
      binOpDef(ops.bitXor, "^", I32Type(), I32Type(), I32Type()),
      binOpDef(ops.div, "/", I32Type(), I32Type(), I32Type()),
      binOpDef(ops.mul, "*", I32Type(), I32Type(), I32Type()),
      unOpDef(ops.neg, "-", I32Type(), I32Type()),
      unOpDef(ops.not, "!", I32Type(), I32Type()),
      binOpDef(ops.rem, "%", I32Type(), I32Type(), I32Type()),
      binOpDef(ops.shl, "<<", I32Type(), I32Type(), I32Type()),
      binOpDef(ops.sub, "-", I32Type(), I32Type(), I32Type()),

      binOpDef(cmp.lt, "<", I32Type() &, I32Type() &, BoolType()),
      binOpDef(cmp.le, "<=", I32Type() &, I32Type() &, BoolType()),
      binOpDef(cmp.gt, ">", I32Type() &, I32Type() &, BoolType()),
      binOpDef(cmp.ge, ">=", I32Type() &, I32Type() &, BoolType()),
      binOpDef(cmp.eq, "==", I32Type() &, I32Type() &, BoolType()),

      // mkFunDef(rc.neu, Seq("value" :: NoType), Rc),
      // mkFunDef(rc.klone, Seq("recv" :: (Rc &)), Rc &),
      // mkFunDef(rc.as_ref, Seq("recv" :: (Rc &)), NoType)
    )
  }

  /* Helpers */

  private def opName(rustName: String): Identifier = Identifier(rustName)
  private def funName(rustName: String): Identifier = Identifier(rustName)
  private def structName(rustName: String): Identifier = Identifier(rustName)

  private def unOpDef(id: Identifier, sym: String, op: Type, result: Type): FunDef =
    mkFunDef(id, Seq("x" :: op), result, Seq(OperatorSymbol(sym)))
  private def binOpDef(id: Identifier, sym: String, lhs: Type, rhs: Type, result: Type): FunDef =
    mkFunDef(id, Seq("lhs" :: lhs, "rhs" :: rhs), result, Seq(OperatorSymbol(sym)))
}

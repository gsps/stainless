/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package rustgen
package generator

// A DSL to create RustTrees programs and expressions
object DSL {
  import rust._

  /* Expressions */

  // implicit class ExprWrapper(expr: Expr) {
  //   ???
  // }

  /* Types */

  def ST(id: Identifier) = new IdToStructType(id)

  class IdToStructType(id: Identifier) {
    def apply(tps: Type*) = StructType(id, tps.toSeq)
  }

  def ET(id: Identifier) = new IdToEnumType(id)

  class IdToEnumType(id: Identifier) {
    def apply(tps: Type*) = EnumType(id, tps.toSeq)
  }

  implicit class TypeWrapper(tpe: Type) {
    def &(): Type = RefType(tpe)
  }

  /* Definitions */

  implicit class TypeToValDef(tp: Type) {
    def ::(nm: String): ValDef = ValDef(Identifier(nm), tp)
  }

  def mkStructDef(id: Identifier, fields: Seq[ValDef]): StructDef =
    StructDef(id, fields)

  def mkEnumDef(id: Identifier, variants: Seq[(Identifier, Seq[ValDef])]): EnumDef = {
    val enumVariants = variants.map { case (vid, fields) => EnumVariant(vid, id, fields) }
    EnumDef(id, enumVariants)
  }

  def mkFunDef(
      id: Identifier,
      params: Seq[ValDef],
      resultType: Type,
      body: Expr,
      flags: Seq[Flag]
  ): FunDef = {
    FunDef(id, params, resultType, body, flags)
  }

  // (For library stubs)
  def mkFunDef(id: Identifier, params: Seq[ValDef], resultType: Type): FunDef = {
    mkFunDef(id, params, resultType, MissingExpr(resultType), Seq(Library))
  }
}

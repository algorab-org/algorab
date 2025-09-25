package org.algorab

import kyo.Chunk
import org.algorab.ast.untpd.Expr
import org.algorab.ast.Identifier
import org.algorab.ast.Type

object Main:

  @main
  def run: Unit =
    
    /* 
    def ast(x: Int): Int =
      if x < 0 then -x
      else x
    */

    val expr = Expr.FunDef(
      name = Identifier("ast"),

      params = Chunk((Identifier("x"), Type.Ref(Identifier("Int")))), 
      
      retType = Type.Ref(Identifier("Int")),

      body = Expr.If(
        Expr.Less(Expr.VarCall(Identifier("x")), Expr.LInt(0)),
        Expr.Minus(Expr.VarCall(Identifier("x"))),
        Expr.VarCall(Identifier("x"))
      )
    )
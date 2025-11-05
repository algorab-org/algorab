package org.algorab

import kyo.*
import org.algorab.parser.Lexer
import scala.io.Source
import java.io.File
import scala.util.Using

object Main extends KyoApp:

  /* 
  72] Block(
[72]   expressions = Seq(
[72]     FunDef(
[72]       name = "id",
[72]       typeParams = Seq("A"),
[72]       params = Seq(("x", Generic(name = "A"))),
[72]       retType = Generic(name = "A"),
[72]       body = Block(
[72]         expressions = Seq(
[72]           FunDef(
[72]             name = "id",
[72]             typeParams = Seq("A$1"),
[72]             params = Seq(("y", Generic(name = "A$1"))),
[72]             retType = Generic(name = "A$1"),
[72]             body = Block(
[72]               expressions = Seq(VarCall(name = "y", exprType = Generic(name = "A$1"))),
[72]               exprType = Generic(name = "A$1")
[72]             ),
[72]             exprType = TypeFun(
[72]               typeParams = Seq("A$1"),
[72]               output = Fun(params = Seq(Generic(name = "A$1")), output = Generic(name = "A$1"))
[72]             )
[72]           ),
[72]           Apply(
[72]             expr = VarCall(
[72]               name = "id",
[72]               exprType = Fun(params = Seq(Generic(name = "A")), output = Generic(name = "A"))
[72]             ),
[72]             args = Seq(VarCall(name = "x", exprType = Generic(name = "A"))),
[72]             exprType = Generic(name = "A")
[72]           )
[72]         ),
[72]         exprType = Generic(name = "A")
[72]       ),
[72]       exprType = TypeFun(
[72]         typeParams = Seq("A"),
[72]         output = Fun(params = Seq(Generic(name = "A")), output = Generic(name = "A"))
[72]       )
[72]     ),
[72]     Apply(
[72]       expr = VarCall(
[72]         name = "id",
[72]         exprType = Fun(params = Seq(Class(name = "Int")), output = Class(name = "Int"))
[72]       ),
[72]       args = Seq(LInt(value = 5, exprType = Class(name = "Int"))),
[72]       exprType = Class(name = "Int")
[72]     )
[72]   ),
[72]   exprType = Class(name = "Int")
[72] )
   */

  run:
    // val code = Source.fromFile("test/resources/golden/good/core019.algo").mkString
    val code = """def id[A](x: A): A =
                 |  def id[A](x: A): A = x
                 |  id[A](x)
                 |id[Int](5)
                 |id[String]("abc")
                 |id[Boolean](true)""".stripMargin
                 
    Console.printLine(compile(code))
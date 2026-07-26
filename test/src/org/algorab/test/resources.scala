package org.algorab.test

import java.net.URI
import java.net.URL
import java.nio.file.FileSystems
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.Files
import java.util.stream.Collectors
import java.util.NoSuchElementException
import scala.collection.JavaConverters.*
import scala.quoted.*
import utest.*
import scala.util.Using
import scala.io.Source
import org.algorab.parsing.ExprParser
import org.algorab.AlgorabProgram
import org.algorab.runProgram

object resources:

  def getResourcePath(url: URL): Path =
    if url.getProtocol == "file" then Paths.get(url.toURI)
    else
      val strings = url.toString.split("!")
      val jarFS = FileSystems.newFileSystem(URI.create(strings(0)), java.util.HashMap())
      jarFS.getPath(strings(1))

  def listResources(folder: String): List[Path] =
    val path = getResourcePath(this.getClass.getResource(folder))
    val ls = Files.list(path)
    ls.collect(Collectors.toList()).asScala.toList

  def readResource(path: String): String =
    Using.resource(Source.fromInputStream(classOf[resources.type].getResourceAsStream(path)))(_.mkString)

  def readResourceLines(path: String): Iterable[String] =
    Using.resource(Source.fromInputStream(classOf[resources.type].getResourceAsStream(path)))(_.getLines().toSeq)

  def runGoldenTest(code: String, input: Iterable[String], expectedOutput: Option[String]): Unit =
    val (errors, output) = AlgorabProgram(runProgram(code))
    assert(errors.isEmpty)
    assert(output.isDefined)

  /** Transparent inline entry point that triggers [[goldenTestsImpl]] at the call site.
    *
    * Inlining is required so that the generated utest `test(…)` calls are placed directly
    * in the surrounding `Tests` block, where utest can register them.
    */
  transparent inline def goldenTests(): Unit =
    ${goldenTestsImpl()}

  /** Compile-time macro that synthesises one `test(filename) { … }` expression per `.algo` file.
    *
    * At compile time:
    *   1. [[listResources]] scans `golden/good/` for `.algo` files.
    *   1. For each file, it lifts the filename, presence of a `.output` sibling, and presence
    *      of a `.input` sibling as `Expr` literals.
    *   1. A `Block` of utest `test(…)` calls is returned, to be spliced at the call site.
    *
    * Adding a new `.algo` file to the resources directory automatically creates a new test
    * case on the next compilation — no manual test registration is required.
    *
    * @return a `Expr[Unit]` whose tree is a block of utest `test(…)` calls
    */
  def goldenTestsImpl()(using Quotes): Expr[Unit] =
    import quotes.reflect.*

    val cases: List[Expr[Unit]] = listResources("/golden/good").filter(_.toString.endsWith(".algo")).map(file =>
      val fileStr = file.getFileName().toString
      val fileName = Expr(fileStr)
      val outputFile = fileStr.substring(0, fileStr.length - 5) + ".output"
      val outputName = Expr(outputFile)
      val hasOutput = Expr(Files.exists(file.resolveSibling(outputFile)))
      val inputFile = fileStr.substring(0, fileStr.length - 5) + ".input"
      val inputName = Expr(inputFile)
      val hasInput = Expr(Files.exists(file.resolveSibling(inputFile)))
      '{
        test($fileName):
          val code = readResource("/golden/good/" + $fileName)
          val expectedOutput =
            if $hasOutput then Some(readResource("/golden/good/" + $outputName))
            else None
          val input =
            if $hasInput then readResourceLines("/golden/good/" + $inputName)
            else Seq.empty
          runGoldenTest(code, input, expectedOutput)
      }
    )

    Block(cases.map(_.asTerm).toList, '{()}.asTerm).asExprOf[Unit]
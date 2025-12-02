package org.algorab

import java.net.URI
import java.net.URL
import java.nio.file.FileSystems
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.Files
import java.util.stream.Collectors
import scala.collection.JavaConverters.*
import kyo.*
import scala.quoted.*
import utest.*
import scala.util.Using
import scala.io.Source

object resources:

  def getResourcePath(url: URL): Path =
    if url.getProtocol == "file" then Paths.get(url.toURI)
    else
      val strings = url.toString.split("!")
      val jarFS = FileSystems.newFileSystem(URI.create(strings(0)), java.util.HashMap())
      jarFS.getPath(strings(1))

  def listResources(folder: String): Chunk[Path] =
    val path = getResourcePath(Main.getClass.getResource(folder))
    val ls = Files.list(path)
    Chunk.from(ls.collect(Collectors.toList()).asScala)

  def readResource(path: String): String =
    Using.resource(Source.fromInputStream(classOf[GoldenTests].getResourceAsStream(path)))(_.mkString)

  def runGoldenTest(code: String, expectedOutput: Maybe[String]): Unit =
    import AllowUnsafe.embrace.danger
    val result = KyoApp.Unsafe.runAndBlock(1.minute)(runCode(code))
    assert(result.isSuccess)

  transparent inline def goldenTests(): Unit =
    ${goldenTestsImpl()}

  def goldenTestsImpl()(using Quotes): Expr[Unit] =
    import quotes.reflect.*

    val cases: Chunk[Expr[Unit]] = listResources("/golden/good").filter(_.toString.endsWith(".algo")).map(file =>
      val fileStr = file.getFileName().toString
      val fileName = Expr(fileStr)
      val outputFile = fileStr.substring(0, fileStr.length - 5) + ".output"
      val outputName = Expr(outputFile)
      val hasOutput = Expr(Files.exists(file.resolveSibling(outputFile)))
      val name = Expr(file.getFileName().toString().dropRight(5))
      '{
        test($fileName):
          val code = readResource("/golden/good/" + $fileName)
          val expectedOutput =
            if $hasOutput then Present(readResource("/golden/good/" + $outputName))
            else Absent
          runGoldenTest(code, expectedOutput)
      }
    )

    Block(cases.map(_.asTerm).toList, '{()}.asTerm).asExprOf[Unit]
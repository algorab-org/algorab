/** Test resource utilities and the compile-time golden-test macro.
  *
  * This object provides:
  *   - [[getResourcePath]] / [[listResources]] / [[readResource]] / [[readResourceLines]] –
  *     classpath-aware helpers for locating and reading test resource files, supporting
  *     both directory-based (development) and JAR-based (CI) layouts.
  *   - [[runGoldenTest]] – executes a compiled Algorab program in an isolated Kyo runtime
  *     with captured stdout and optional stdin, then asserts the output matches the expectation.
  *   - [[goldenTests]] / [[goldenTestsImpl]] – a `transparent inline` / macro pair that
  *     discovers all `.algo` files in `golden/good/` at compile time and synthesises a
  *     utest `Tests` block containing one `test(filename)` per file.
  */
package org.algorab

import java.net.URI
import java.net.URL
import java.nio.file.FileSystems
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.Files
import java.util.stream.Collectors
import java.util.NoSuchElementException
import scala.collection.JavaConverters.*
import kyo.*
import scala.quoted.*
import utest.*
import scala.util.Using
import scala.io.Source

object resources:

  /** Resolves a classpath resource [[URL]] to a [[Path]] that can be used with `java.nio.file`.
    *
    * Handles both the common case (plain `file://` URL pointing to a directory on disk) and
    * the JAR case (`jar:file://…!/path`), where a temporary [[java.nio.file.FileSystem]] is
    * created for the JAR so that directory listing and file reading work uniformly.
    *
    * @param url the classpath resource URL to resolve
    * @return a `java.nio.file.Path` that can be passed to `Files.list`, `Files.readAllBytes`, etc.
    */
  def getResourcePath(url: URL): Path =
    if url.getProtocol == "file" then Paths.get(url.toURI)
    else
      val strings = url.toString.split("!")
      val jarFS = FileSystems.newFileSystem(URI.create(strings(0)), java.util.HashMap())
      jarFS.getPath(strings(1))

  /** Lists all immediate children of the classpath resource directory `folder`.
    *
    * @param folder the resource-relative folder path (e.g. `"/golden/good"`)
    * @return a [[Chunk]] of `Path`s for each direct child of the folder
    */
  def listResources(folder: String): Chunk[Path] =
    val path = getResourcePath(this.getClass.getResource(folder))
    val ls = Files.list(path)
    Chunk.from(ls.collect(Collectors.toList()).asScala)

  /** Reads and returns the full contents of a classpath resource as a `String`.
    *
    * @param path the resource-relative path (e.g. `"/golden/good/core001.algo"`)
    * @return the file contents as a `String`
    */
  def readResource(path: String): String =
    Using.resource(Source.fromInputStream(classOf[GoldenTests].getResourceAsStream(path)))(_.mkString)

  /** Reads the lines of a classpath resource as an `Iterable[String]`.
    *
    * The lines do not include line terminators.
    *
    * @param path the resource-relative path
    * @return the file lines as a `Seq[String]`
    */
  def readResourceLines(path: String): Iterable[String] =
    Using.resource(Source.fromInputStream(classOf[GoldenTests].getResourceAsStream(path)))(_.getLines().toSeq)

  /** Executes an Algorab program and asserts that its stdout matches `expectedOutput`.
    *
    * The program is run with:
    *   - Stdout captured via `Console.withOut`.
    *   - Stdin supplied from `input` via `Console.withIn`.
    *   - A one-minute overall timeout via `KyoApp.Unsafe.runAndBlock`.
    *
    * The test fails if:
    *   - The program produces a `Result.Failure` (compilation error – should not happen for
    *     files in `golden/good/`).
    *   - The captured stdout does not match `expectedOutput` (after stripping leading/trailing
    *     whitespace from both sides).
    *
    * @param code           the Algorab source code to run
    * @param input          lines to supply as stdin (empty for programs that do not read input)
    * @param expectedOutput the expected stdout, or `Absent` if no output check is needed
    */
  def runGoldenTest(code: String, input: Iterable[String], expectedOutput: Maybe[String]): Unit =
    import AllowUnsafe.embrace.danger
    val result = KyoApp.Unsafe.runAndBlock(1.minute) {
      Console.withOut(Console.withIn(input)(runCode(code))).map((out, r) =>
          r.map((out, _))
      )
    }.flatten

    assert(result.isSuccess)
    val out = result.getOrElse(throw NoSuchElementException("Result"))._1

    assert(expectedOutput.forall(str => str.strip == out.stdOut.strip))

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

    val cases: Chunk[Expr[Unit]] = listResources("/golden/good").filter(_.toString.endsWith(".algo")).map(file =>
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
            if $hasOutput then Present(readResource("/golden/good/" + $outputName))
            else Absent
          val input =
            if $hasInput then readResourceLines("/golden/good/" + $inputName)
            else Seq.empty
          runGoldenTest(code, input, expectedOutput)
      }
    )

    Block(cases.map(_.asTerm).toList, '{()}.asTerm).asExprOf[Unit]

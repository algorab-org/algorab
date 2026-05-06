/** Golden-file test suite for the Algorab compiler and runtime.
  *
  * Each test case is derived at compile time from a `.algo` source file in
  * `main/test/resources/golden/good/`.  For each file the suite:
  *   1. Reads the source code from the corresponding `.algo` file.
  *   1. Optionally reads expected output from a `.output` file (same base name).
  *   1. Optionally supplies stdin lines from a `.input` file (same base name).
  *   1. Calls [[resources.runGoldenTest]] to execute the program and assert correctness.
  *
  * The test cases are discovered and generated at compile time by the
  * [[resources.goldenTestsImpl]] macro, so adding a new `.algo` file to the resources
  * directory automatically creates a new test case on the next compilation.
  */
package org.algorab

import kyo.*
import java.io.IOException
import scala.io.Source
import java.nio.file.Files
import utest.*
import scala.quoted.*
import scala.annotation.nowarn

/** utest `TestSuite` that runs all golden file tests. */
class GoldenTests extends TestSuite:

  /** The test tree generated at compile time from the resource directory.
    *
    * The `@nowarn("msg=pure")` suppresses a spurious warning about a pure expression
    * in statement position emitted by the macro expansion.
    */
  @nowarn("msg=pure")
  val tests = Tests:
    resources.goldenTests()

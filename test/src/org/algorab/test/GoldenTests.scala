package org.algorab.test

import utest.*
import scala.annotation.nowarn

class GoldenTests extends TestSuite:

  @nowarn("msg=pure")
  val tests = Tests:
    resources.goldenTests()
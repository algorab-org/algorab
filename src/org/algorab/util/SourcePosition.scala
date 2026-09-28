package org.algorab.util

case class SourcePosition(file: String, start: (line: Int, column: Int), end: (line: Int, column: Int))
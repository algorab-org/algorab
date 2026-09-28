package org.algorab.util

case class SourcePosition(file: FileName, start: (line: Int, column: Int), end: (line: Int, column: Int))
package org.algorab

import mainargs.Compat
import mainargs.TokensReader
import purelogic.*
import org.algorab.util.FileName

private val line: String = "-" * 80

given TokensReader.Simple[os.Path] with
  override def shortName: String = "path"
  override def read(strs: Seq[String]): Either[String, os.Path] = Right(os.Path(strs.head, os.pwd))

private def getSources(files: Seq[os.Path]): Abort[String] ?=> Seq[(FileName, String)] =
  def readSource(path: os.Path): (FileName, String) =
    (FileName.assume(path.subRelativeTo(os.pwd).toString), os.read(path))

  val invalidPaths = files.filterNot(os.exists)
  if invalidPaths.isEmpty then 
    files.flatMap(path =>
      if os.isDir(path) then os.walk(path).map(readSource)
      else Seq(readSource(path))
    )
  else
    fail(s"The following files do not exist: ${invalidPaths.mkString("\n- ", "\n- ", "")}")

def exitOnAbort[A](program: Abort[String] ?=> A): A =
  recover(program)(error =>
    Console.err.println(error)
    Compat.exit(-1)
  )

def runAlgorab[A](program: Abort[String] ?=> AlgorabProgram[A]): Unit = exitOnAbort:
  val result = AlgorabProgram(program)

  if result.errors.isEmpty then ()
  else fail(Console.RED + result.printedErrors.mkString(s"$line\n", s"\n$line\n", s"\n$line") + Console.RESET)

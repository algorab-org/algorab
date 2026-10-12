package org.algorab

import mainargs.*

object Main:

  @main(doc = "Run an Algorab program.")
  def run(
    @arg(
      doc = "The source files and directories of the program",
      positional = true
    )
    files: Seq[os.Path]
  ): Unit = runAlgorab:
    val sources = getSources(files)
    println(s"Running ${sources.size} files...")
    runProgram(sources*)

  @main(doc = "Analyze an Algorab program without running it.")
  def analyze(
    @arg(
      doc = "The source files and directories of the program",
      positional = true
    )
    files: Seq[os.Path]
  ): Unit = runAlgorab:
    val sources = getSources(files)
    println(s"Analyzing ${sources.size} files...")
    analyzeProgram(sources*).asInstanceOf[Unit]
    println("Done analyzing.")

  def main(args: Array[String]): Unit = Parser(this).runOrExit(args.toSeq, printHelpOnExit = true): Unit
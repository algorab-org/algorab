package org.algorab

import kyo.ParseFailure
import org.algorab.typer.TypeFailure

type CompilerFailure = ParseFailure | TypeFailure

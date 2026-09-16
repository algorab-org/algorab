package org.algorab.util

enum ConsoleError:
  case InvalidInt(got: String)
  case InvalidFloat(got: String)
  case EndOfInput
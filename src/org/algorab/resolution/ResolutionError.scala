package org.algorab.resolution

import org.algorab.ast.Identifier

enum ResolutionError:
  case UnknownName(name: Identifier)
  case ForwardDeclaration(name: Identifier)
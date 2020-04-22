/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.tasty.bridge

import scala.tools.nsc
import nsc.symtab, nsc.tasty.TastyUniverse

abstract class TastyCore { self: TastyUniverse =>
  import self.{symbolTable => u}

  // Compiler Entry Point
  type SymbolTable <: symtab.SymbolTable { def settings: nsc.Settings }
  val symbolTable: SymbolTable

  // Misc
  type Constant   = u.Constant
  type Annotation = u.Annotation
  type Type       = u.Type
  type Symbol     = u.Symbol

  // Trees
  type Tree  = u.Tree
  type Ident = u.Ident // wraps a mangled name

  private val Identity = (x: Any) => x

  def id[T]: T => T = Identity.asInstanceOf[T => T]
  def map[T, U](ts: List[T], f: T => U): List[U] = if (f `eq` Identity) ts.asInstanceOf[List[U]] else ts.map(f)

}

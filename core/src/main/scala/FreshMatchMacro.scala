/*
 * Copyright (c) 2013, Martin Zuber
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above
 *   copyright notice, this list of conditions and the following
 *   disclaimer in the documentation and/or other materials provided
 *   with the distribution.
 * - Neither the name of the TU Berlin nor the names of its
 *   contributors may be used to endorse or promote products derived
 *   from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

import scala.language.experimental.macros
import scala.reflect.macros.Context

import Fresh._

/**
  * Macro implementation for pattern matching over abstraction values.
  *
  * This macro performs a syntax transformation for each regular
  * case pattern into a case pattern with explicit swapping.
  */
object FreshMatchMacro {

  /**
    * Pattern matching over abstraction values.
    *
    * This macro performs explicit swapping for all abstraction patterns in a case pattern, i.e.,
    * the pattern matching code
    * {{{
    * freshMatch(expr){
    *   case Constructor(Abstraction(y, e)) => Abstraction(y, ... e ...)
    * }
    * }}}
    * will be transformed into case patterns with explicit swapping:
    * {{{
    * expr match {
    *   case Constructor(Abstraction(y, e)) => {
    *     val z = fresh()
    *     Abstraction(z, swap(z, y, ... e ...))
    *   }
    * }
    * }}}
    */
  def freshMatchImpl[A: c.WeakTypeTag, B: c.WeakTypeTag](c: Context)(expr: c.Expr[A])(patterns: c.Expr[PartialFunction[A, B]]): c.Expr[B] = {
    import c.universe._

    /* Perform explicit swapping transformation on all case definitions */
    val transformer = new Transformer {
      override def transformCaseDefs(trees: List[CaseDef]) = trees map {
	case caseDef => caseDef
      }
    }

    /* Construct anonymus partial function with transformed case patterns */
    val transformedPartialFunction = transformer.transform(patterns.tree)
    println(show(transformedPartialFunction))

     c.Expr[B](q"$transformedPartialFunction($expr)")
  }

}

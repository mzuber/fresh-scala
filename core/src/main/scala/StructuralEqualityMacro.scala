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
  * Macro for testing structural equality of two terms of the object-language.
  *
  * The important property of out system is that values of a type using abstraction types
  * are observationally equivalent iff they correspond to α-equivalence terms of the
  * object-language. Thus, a function that tests for structural equality computes object-
  * level α-equivalence.
  */
object StructuralEqualityMacro {

  /**
    * Structural equality over two terms of the object-language.
    *
    * This macro generates a match statement with patterns for pairs of all constructors
    * of the given algebraic data type. The body of each case definition compares the
    * children of each value, swapping bound variables in all abstraction values.
    */
  def structuralEqualityImpl[A: c.WeakTypeTag](c: Context)(e1: c.Expr[A], e2: c.Expr[A]): c.Expr[Boolean] = {
    import c.universe._

    // Dummy: true
    reify(true)
  }

}

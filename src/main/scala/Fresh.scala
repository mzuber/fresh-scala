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

import NameSupply.freshName

/**
  * Define object-level syntax modulo α-equivalence in Scala.
  */
object Fresh {

  /**
    * A class for atoms, i.e., the atomic unit for fresh names.
    *
    * Each instance of this class encapulates a unique name.
    */
  class Atom {
    private val atom: String = freshName("name_")

    override def toString: String = atom
  }

  /**
    * A class for bindable names in the object language.
    */
  case class Name[T](atom: Atom) {

    /**
      * Check, if this bindable name occurs in the algebraic suport of the given expression.
      */
    def freshfor[S](expr: S): Boolean = true
  }


  /**
    * Create a fresh bindable name.
    */
  def fresh[T](): Name[T] = Name[T](new Atom)


  /**
    * A class for abstractions.
    */
  class Abstraction[S, T](boundName: Name[S], expr: T) {

    // TODO: Some magic unapply function
  }


  /**
    * Swap bindable names in an expression.
    *
    * Interchange all occurrences of the given atoms in the given expression.
    */
  def swap[S, T](name: Name[S], and: Name[S], in: T) = {

  }

}

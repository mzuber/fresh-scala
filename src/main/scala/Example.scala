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

import Fresh._


/**
  * Example use of the FreshScala system for the object-language from
  * "Mark R. Shinwell, Andrew M. Pitts - Fresh Objective Caml User Manual".
  */
object Example {

  /* A type for variable names */
  class Var


  /**
    * Abstract syntax for our object-language.
    *
    * Using bindable names allows us to define α-equivalence
    * classes of the languages abstract syntax trees.
    */
  abstract class Term
  case class Variable(name: Name[Var]) extends Term                                              /* x */
  case class Function(function: Abstraction[Var, Term]) extends Term                             /* fn x => e */
  case class Application(function: Term, argument: Term) extends Term                            /* e1 e2 */
  case class LetFunction(letFun: Abstraction[Var, (Abstraction[Var, Term], Term)]) extends Term  /* let fun f x = e1 in e2 */


  /**
    * Capture-avoiding substitution.
    *
    * This method computes (a representation of) the object-level term
    * obtained by capture-avoiding substitution of the term 'e1' for all
    * free occurrences of the variable 'x' in the term 'e2'.
    */
  def subst(e1: Term, x: Name[Var], e2: Term): Term = e2 freshMatch {
    case Variable(y) => if (x == y) e1 else Variable(y)
    case Function(Abstraction(y, e)) => Function(<<(y)>> subst(e1, x, e))
    case Application(f, e) => Application(subst(e1, x, f), subst(e1, x, e))
    case LetFunction(Abstraction(f, (Abstraction(y, e), body))) =>
      LetFunction(<<(f)>> (<<(y)>> subst(e1, x, e), subst(e1, x, body)))
  }


  /**
    * Capture-avoiding substitution (explicit version).
    *
    * This method computes (a representation of) the object-level term
    * obtained by capture-avoiding substitution of the term 'e1' for all
    * free occurrences of the variable 'x' in the term 'e2'.
    * This implementation doesn't make use of abstraction patterns, but
    * performs the freshening of the bound names explicitly.
    */
  def substExpl(e1: Term, x: Name[Var], e2: Term): Term = e2 match {
    case Variable(y) => if (x == y) e1 else Variable(y)
    case Function(Abstraction(y, e)) => {
      val z: Name[Var] = fresh()
      Function(<<(z)>> swap(z, y, substExpl(e1, x, e)))
    }
    case Application(f, e) => Application(substExpl(e1, x, f), substExpl(e1, x, e))
    case LetFunction(Abstraction(f, (Abstraction(y, e), body))) => {
      val g: Name[Var] = fresh()
      val z: Name[Var] = fresh()
      LetFunction(<<(g)>> swap(g, f, (<<(z)>> swap(z, y, substExpl(e1, x, e)), substExpl(e1, x, body))))
    }
  }
}

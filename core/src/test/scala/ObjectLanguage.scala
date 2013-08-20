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
  * Object-language from "Mark R. Shinwell, Andrew M. Pitts - Fresh Objective Caml User Manual".
  */
object ObjectLanguage {

  /* A type for identifiers */
  class Ide


  /**
    * Abstract syntax for our object-language.
    *
    * Using bindable names allows us to define α-equivalence
    * classes of the languages abstract syntax trees.
    */
  sealed abstract class Term
  case class Var(name: Name[Ide]) extends Term                                        /* x */
  case class Fun(fun: Abstraction[Ide, Term]) extends Term                            /* fn x => e */
  case class App(fun: Term, arg: Term) extends Term                                   /* e1 e2 */
  case class Let(let: Abstraction[Ide, (Abstraction[Ide, Term], Term)]) extends Term  /* let fun f x = e1 in e2 */


  /**
    * Capture-avoiding substitution.
    *
    * This method computes (a representation of) the object-level term
    * obtained by capture-avoiding substitution of the term 'e1' for all
    * free occurrences of the variable 'x' in the term 'e2'.
    */
  def subst(e1: Term, x: Name[Ide], e2: Term): Term = freshMatch(e2){
    case Var(y) => if (x == y) e1 else Var(y)
    case Fun(Abstraction(y, e)) => Fun(<<(y)>> subst(e1, x, e))
    case App(f, e) => App(subst(e1, x, f), subst(e1, x, e))
    case Let(Abstraction(f, (Abstraction(y, e), body))) =>
      Let(<<(f)>> (<<(y)>> subst(e1, x, e), subst(e1, x, body)))
  }
}

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

import org.kiama.rewriting.Rewriter._

import AtomSupply.freshAtom

/**
  * Define object-level syntax modulo α-equivalence in Scala.
  */
object Fresh {

  /**
    * A value class for atoms, i.e., the atomic unit for fresh names.
    */
  class Atom(val atom: Int) extends AnyVal {
    override def toString = atom.toString
  }


  /**
    * A class for bindable names in the object language.
    */
  case class Name[A](atom: Atom) {

    /**
      * Check, if this bindable name occurs in the algebraic support of the given expression.
      *
      * This method provides a 'not-a-free-variable-off' test for object-level terms.
      */
    def freshfor[B](expr: B): Boolean = true

    /**
      * A textual representation of this name in the form of `name_`''n'',
      * with distinct atoms getting distinct numbers ''n''.
      */
    override def toString: String = "name_" + atom
  }


  /**
    * Create a fresh bindable name with a unique atom.
    */
  def fresh[A](): Name[A] = Name[A](freshAtom())


  /**
    * A class for abstractions.
    */
  case class Abstraction[A, B](boundName: Name[A], expr: B) {

    /**
      * Concrete this abstraction at a (fresh) atom.
      */
    def concreteAt(atom: Name[A]): B = swap(boundName, atom, expr)
  }


  /**
    * Syntactic sugar for defining abstraction values.
    *
    * This class allows the user to define abstraction values using a syntax close
    * to the one found in the FreshML language, i.e.,
    * {{{
    * val expr: B = ...
    * val x: Name[A] = fresh()
    * val abs: Abstraction[A, B] = <<(x)>> expr
    * }}}
    */
  case class AbstractionBuilder[A](name: Name[A]) {
    def >>[B](expr: B): Abstraction[A, B] = Abstraction[A, B](name, expr)
  }

  /**
    * Syntactic sugar for defining abstraction values.
    *
    * This method lifts a name into an AbstractionBuilder, which can be used to
    * define an abstraction using a syntax close to the one found in the FreshML language.
    */
  def <<[A](name: Name[A]): AbstractionBuilder[A] = AbstractionBuilder[A](name)


  /**
    * Swap bindable names in an expression.
    *
    * Interchange all occurrences of the given atoms in an expression which evaluates to a value.
    */
  def swap[A, B](name1: Name[A], name2: Name[A], value: B): B = {
    /* Swapping in abstractions */
    if (expr.isInstanceOf[Abstraction[A, B]]) {
      val Abstraction(name, e) = expr.asInstanceOf[Abstraction[A, B]]
      // ...
    }
    expr // Temporary dummy for the type checker
  }


  /**
    * Replace every occurrence of the atom 'a' with the atom 'b' in the given expression.
    */
  private def replace[A, B](a: Name[A], b: Name[A], expr: B): B = {
    // Define a Kiama strategy which replaces every atom `a' with `b'
    val replace = rule { case atom: Name[A] if atom == a => b }

    // Apply the strategy to the given expression and make shure the result has type B
    replace(expr).getOrElse(expr) match {
      case expr: B @unchecked => expr
    }
  }


  /**
    * Swap multiple names in an expression.
    *
    * Both lists must be of the same length.
    *
    * @return The value formed by swapping the first element of 'names1' with the
    *         first element of 'names2' throughout the given expression, and so
    *         forth to the end of the lists.
    */
  def swap[A, B](names1: List[Name[A]], names2: List[Name[A]], expr: B): B = {
    // Macro magic ftw
    expr // Temporary dummy for the type checker
  }


  /**
    * An class for pattern matching over abstractions.
    *
    * This implicit class allows the user to define a match statement
    * over abstractions values in the regular, infix fashion.
    */
  implicit class FreshMatch[A](expr: A) {
    
    /**
      *  Pattern matching over abstraction values.
      */
    def freshMatch[B](patterns: PartialFunction[A, B]): B = {
      // call a macro which performs the corresponding transformations
      patterns(expr) // Temporary dummy for the type checker
    }
  }


  /**
    * A class for determining structural equality of two expressions.
    */
  implicit class StructuralEquality[A](expr: A) {

    /**
      * Test if this expression is structurally equal to that
      * expression, i.e., check for object-level α-equivalence.
      */
    def ~(that: A): Boolean = true
  }

}
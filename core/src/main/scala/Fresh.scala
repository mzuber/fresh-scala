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
import scala.annotation.StaticAnnotation

import org.kiama.rewriting.Rewriter._

import AtomSupply.freshAtom
import FreshMatchMacro.freshMatchImpl
import FreshAnnotation.freshAnnotationImpl

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
      * This method provides a 'not-a-free-variable-of' test for object-level terms.
      */
    def freshfor[B](expr: B): Boolean = {

      def boundNames = collects {
	case abstraction: Abstraction[A, _] => abstraction.boundName
      }

      boundNames(expr) contains this
    }

    /**
      * Create a name with a fresh atom and the same type as this name.
      *
      * Note: This method does not change the atom of this name, it creates
      * a new name of the same type with a fresh atom.
      */
    def refresh(): Name[A] = fresh[A]()

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
    * define an abstraction using a syntax close to the one found in the FreshML
    * language, i.e.,
    * {{{
    * val expr: B = ...
    * val x: Name[A] = fresh()
    * val abs: Abstraction[A, B] = <<(x)>> expr
    * }}}
    */
  def <<[A](name: Name[A]): AbstractionBuilder[A] = AbstractionBuilder[A](name)


  /**
    * Swap bindable names in an expression.
    *
    * Interchange all occurrences of the given atoms in an expression, i.e.,
    * replace every occurrence of the first atom with the second one and vice versa.
    */
  def swap[A, B](a: Name[A], b: Name[A], expr: B): B = {
    // Define a Kiama strategy which replaces every atom `a' with `b' and every atom `b' with `a'
    val swap = rule {
      case atom: Name[A] if atom == a => b
      case atom: Name[A] if atom == b => a
    }

    // Apply the strategy to the given expression and make shure the result has type B
    everywhere(swap)(expr).getOrElse(expr).asInstanceOf[B]
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
    val names = names1 zip names2
    
    /*
     * Swap atoms in the given lists from left to right, i.e.,
     * ... swap(..., ..., swap(a2, b2, swap(a1, b1, expr)
     */
    names.foldLeft(expr){ case (expr, (a, b)) => swap(a, b, expr) }
  }


  /**
    * Pattern matching over abstractions values.
    */
  def freshMatch[A, B](expr: A)(patterns: PartialFunction[A, B]): B = macro freshMatchImpl[A,B]


  /**
    * An annotation for pattern matching over abstraction values.
    */
  class Fresh extends StaticAnnotation {
    def macroTransform(annottees: Any*) = macro freshAnnotationImpl
  }


  /**
    * Generic implementation for testing structural equality of two terms of the object-language.
    *
    * The important property of out system is that values of a type using abstraction types
    * are observationally equivalent iff they correspond to α-equivalence terms of the
    * object-language. Thus, a function that tests for structural equality computes object-
    * level α-equivalence.
    *
    * This method tests if the first expression is structurally equal to the second one
    * using data-type generic programming.
    */
  def structuralEquality[A](e1: A, e2: A): Boolean = {

    /* Get the children of the given term. */
    def getChildren(term: Any): List[Any] = {
      var children = List[Any]()

      all(queryf {
	case c => children = children :+ c
      })(term)

      children
    }

    val genericEquality = rule {
      /*
       * If both terms are abstraction values, we swap every occurrence
       * of `x1' in `e1' with `x2' and compare the result to `e2'.
       */
      case (Abstraction(x1, e1), Abstraction(x2, e2)) => structuralEquality(swap(x1 ,x2 ,e1 ), e2)

      /*
       * Generic comparison of two terms.
       * Two terms are equal if:
       * - they are instances of the class
       * - have the same number of children
       * - all children are equal to their matching counterpart
       */
      case (t1, t2) if t1.getClass == t2.getClass => {
        val c1 = getChildren(t1)
        val c2 = getChildren(t2)

        if (c1.size != c2.size) false
        else if (c1.size == 0) t1 == t2
	// TODO: Can this be rewritten using aggregate?
        else (c1, c2).zipped.foldLeft(true) {
	  case (acc, (x, y)) => acc && structuralEquality(x, y)
	}
      }

      /* Otherwise, the two terms are not structurally equal */
      case _ => false
    }

    genericEquality((e1, e2)).get.asInstanceOf[Boolean]
  }


  /**
    * Syntactic sugar for checking if two expressions are structurally equal.
    */
  implicit class ObjectLevelAlphaEquivalence[A](expr: A) {

    /**
      * Test if this expression is structurally equal to the
      * given one, i.e., check for object-level α-equivalence.
      */
    def =:=(that: A): Boolean = structuralEquality[A](expr, that)

    /**
      * Test if this expression is not structurally equal to the given one.
      */
    def =/=(that: A): Boolean = ! structuralEquality[A](expr, that)
  }
}

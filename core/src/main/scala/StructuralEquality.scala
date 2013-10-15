
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
import org.kiama.rewriting._
import scala.collection.mutable.ListBuffer
import Fresh._

/**
  * Generic implementation for testing structural equality of two terms of the object-language.
  *
  * The important property of out system is that values of a type using abstraction types
  * are observationally equivalent iff they correspond to α-equivalence terms of the
  * object-language. Thus, a function that tests for structural equality computes object-
  * level α-equivalence.
  */
object StructuralEquality extends Rewriter {

  def structuralEquality[A](e1: A, e2: A): Boolean = {
    eq(e1,e2)
  }

  private def eq[A](a:A,b:A) : Boolean = {
    val eqRule = rule {
      /** If both sides have the form Abstraction(x1,e1) respectively 
        * Abstraction(x2,e2) we swap every occurrence of x1 in e1 with x2.
        * This new term is compared to e2.
        */
      case ((a @ Abstraction(x1,e1)),(b @ Abstraction(x2,e2))) => 
        eq(swap(x1,x2,e1),e2)
       /** This part implements the generic comparison of to terms:
         * Two terms are equal:
         *  -if they are instances of the class
         *  -have the same number of children
         *  -all children are equal with their matching counterpart
         */
      case (x,y) if x.getClass == y.getClass =>
        val xc = getChildren(x)
        val yc = getChildren(y)
        if(xc.size != yc.size) false
        else if(xc.size == 0) x == y //true would probably be sufficient
        else xc.zip(yc).foldLeft(true)({ case (acc,(f,g)) => acc && eq(f,g)})
      case _ => false
    }
    eqRule((a,b)).get.asInstanceOf[Boolean]
  }


  /**
    * Get the children of the given value.
    */
  private def getChildren(x: Any): List[Any] = {
    val l = new ListBuffer[Any]()
    all(queryf {
      case a => l += a
    })(x)
    l.toList
  }

}
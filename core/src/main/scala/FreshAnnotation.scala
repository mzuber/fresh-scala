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

import scala.language.reflectiveCalls
import scala.language.experimental.macros
import scala.reflect.macros.Context

import Fresh._

/**
  * Macro annotation for pattern matching over abstraction values.
  *
  * This annotation performs a syntax transformation for each regular
  * case pattern into a case pattern with explicit swapping.
  */
object FreshAnnotation {

  /**
    * Pattern matching over abstraction values.
    *
    * This annotation performs explicit swapping for all abstraction patterns in a case pattern, i.e.,
    * the pattern matching code in the definition
    * {{{
    * @Fresh
    * def f(expr: T) = expr match {
    *   case Constructor(Abstraction(y, e)) => Abstraction(y, ... e ...)
    * }
    * }}}
    * will be transformed into case patterns with explicit swapping:
    * {{{
    * def f(expr: T) = expr match {
    *   case Constructor(Abstraction(y, e)) => {
    *     val z = fresh()
    *     Abstraction(z, swap(z, y, ... e ...))
    *   }
    * }
    * }}}
    */
  def freshAnnotationImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val definition = annottees.head.tree
    // println(showRaw(definition))

    /* Perform explicit swapping transformation on all case definitions */
    val eplicitSwappingTransformer = new Transformer {

      override def transformCaseDefs(trees: List[CaseDef]) = trees map {
	case CaseDef(pattern, guard , body) => {

	  /* Collect all pattern variables which are bound in an abstraction pattern */
	  val boundNameTraverser = new Traverser {

	    var boundNames: List[Name] = List()

	    override def traverse(tree: Tree) = tree match {
	      case pat @ pq"Abstraction($name @ ${_}, $body)" => {
		boundNames = boundNames :+ name
		super.traverse(body)
	      }
	      case x => super.traverse(tree)
	    }
	  }

	  boundNameTraverser.traverse(pattern)
	  val boundNames = boundNameTraverser.boundNames

	  /*
	   * Construct value definitions which generate fresh names for each bound
	   * pattern variable, e.g., for the bound pattern variables `x' and `y' we
	   * produce the following code block, where `z1' and `z2' are fresh variable
	   * names:
	   * {
	   *   val z1: Name[A] = fresh()
	   *   val z2: Name[A] = fresh()
	   * }
	   */
	  val freshNames: Map[Name, TermName] = boundNames.map((name: Name) => (name, newTermName(c.fresh("freshName")))).toMap
	  val freshNameDefs: List[ValDef] = freshNames.values.toList map {
	    case name: TermName => q"val $name : Name[Any] = fresh()"
	  }

	  /*
	   * Transform the body of the case definition such that:
	   * - Each of the names bound in an abstraction pattern will be associated
	   *   with one of the freshly generated names.
	   * - The expression of each abstraction value will be surrounded by a swap
	   *   call which swaps the bound name with its corresponding fresh one.
	   * If for example the bound pattern variable `x' is associated with a
	   * fresh name stored in the variable `z', the expression
	   * {
	   *  Abstraction(x, e)
	   * }
	   * will be transformed into
	   * {
	   *  Abstraction(z, swap(z, x, e))
	   * }
	   */
	  val transformedBody = body
	  
	  CaseDef(pattern, guard, Block(freshNameDefs, transformedBody))
	}
      }
    }

    /* Construct anonymus partial function with transformed case patterns */
    val transformedDefinition = eplicitSwappingTransformer.transform(definition)

    c.Expr[Any](definition)
  }

}

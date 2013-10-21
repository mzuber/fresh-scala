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

import scala.language.postfixOps
import scala.language.reflectiveCalls
import scala.language.experimental.macros
import scala.reflect.macros.Context
import scala.collection.mutable.ListBuffer

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
    *   case Constructor(Abstraction(y, ...)) => ...
    * }
    * }}}
    * will be transformed into case patterns with explicit swapping:
    * {{{
    * expr match {
    *   case Constructor(Abstraction(y, e @ ...)) => {
    *     val z = fresh()
    *     Constructor(Abstraction(z, swap(z, y, e))) match {
    *       case Constructor(Abstraction(y, ...)) => ...
    *     }
    *   }
    * }
    * }}}
    */
  def freshMatchImpl[A: c.WeakTypeTag, B: c.WeakTypeTag](c: Context)(expr: c.Expr[A])(patterns: c.Expr[PartialFunction[A, B]]): c.Expr[B] = {
    import c.universe._
    /* Copy from FreshAnnotation */
    /* TODO Use macro traits in 2.11 */
    /* Perform explicit swapping transformation on all case definitions */
    val eplicitSwappingTransformer = new Transformer {

      override def transformCaseDefs(trees: List[CaseDef]) = trees map {
	case CaseDef(pattern, guard , body) => {

	  /*
	   * Rewrite the pattern, such that
	   * - the body of an abstraction is aliased with a pattern variable,
	   * - and all wildcards are aliased with a pattern variable.
	   * For example, the pattern
	   * {
	   *  C(Abstraction(x, ...), _)
	   * }
	   * will be transformed into
	   * {
	   *  C(Abstraction(x, e1 @ ...), e2)
	   * }
	   * Additionally, we collect all pattern variables which are bound in an
	   * abstraction pattern along the way. The prefix for all new pattern variables,
	   * i.e., the generated aliases, will be '_e_'.
	   */
	  val patternTransformer = new Transformer {
	    /* The pattern variables which are bound in an abstraction pattern and the alias of the corresponding body */
	    var abstractions: Map[Name, Name] = Map()

	    /* Pattern variables of the bound names in the order they appear in the pattern */
	    var boundNames: List[Name] = List()

	    override def transform(tree: Tree) = tree match {
	      /* Abstraction pattern with aliased body */
	      case pq"Abstraction($name @ ${_}, $alias @ $body)" => {
		abstractions = abstractions + (name -> alias)
		boundNames = boundNames :+ name
		pq"Abstraction($name, $alias @ ${super.transform(body)})"
	      }
	      /* Abstraction pattern with non-aliased body */
	      case pq"Abstraction($name @ ${_}, $body)" => {
		val alias = newTermName(c.fresh("_e_"))
		abstractions = abstractions + (name -> alias)
		boundNames = boundNames :+ name
		pq"Abstraction($name, $alias @ ${super.transform(body)})"
	      }
	      /* Aliased wildcards are not transformed */
	      case pat @ pq"$name @ ${Ident(nme.WILDCARD)}" => pat

	      /* Wildcards are aliased with a fresh pattern variable */
	      case pq"_" => {
		val alias = newTermName(c.fresh("_e_"))
		pq"$alias @ _"
	      }

	      case _ => super.transform(tree)
	    }
	  }
	  val transformedPattern = patternTransformer.transform(pattern)
	  val boundNames = patternTransformer.boundNames
	  val abstractions = patternTransformer.abstractions

	  /*
	   * Construct value definitions which generate fresh names for each bound
	   * pattern variable, e.g., for the bound pattern variables `x' and `y' we
	   * produce the following code block, where `z1' and `z2' are fresh variable
	   * names:
	   * {
	   *   val z1 = x.refresh()
	   *   val z2 = y.refresh()
	   * }
	   * The prefix for all fresh variable names will be '_$_'.
	   */
	  val freshNames: Map[Name, TermName] = boundNames map {
	    case name => (name, newTermName(c.fresh("_$_")))
	  } toMap
	  val freshNameDefs: List[ValDef] = freshNames.toList map {
	    case (boundName, freshName) => q"val $freshName = $boundName.refresh()"
	  }

	  /*
	   * Transform the case definition such that:
	   * - Each of the names bound in an abstraction pattern will be associated
	   *   with one of the freshly generated names.
	   * - The expression bound in the abstraction pattern will be surrounded by
	   *   a swap call which swaps the bound name with its corresponding fresh one.
	   * 
	   * If for example the bound pattern variable `x' is associated with a
	   * fresh name stored in the variable `z', the case definition
	   * {
	   *  case Abstraction(x, e @ ...) => ...
	   * }
	   * will be transformed into
	   * {
	   *  case Abstraction(x, e @ ...) => {
	   *    val z = x.refresh()
	   *    Abstraction(z, swap(z, x, e)) match {
	   *      case Abstraction(x, e @ ...) => ...
	   *    }
	   *  }
	   * }
	   * 
	   * Nested abstraction patterns will be transformed the same way, the case
	   * definition
	   * {
	   *  case Abstraction(x, e1 @ Abstraction(y, e2 @ ...)) => ...
	   * }
	   * will be translated into
	   * {
	   *  case Abstraction(x, e1 @ Abstraction(y, e2 @ ...)) => {
	   *    val z1 = x.refresh()
	   *    val z2 = y.refresh()
	   *    Abstraction(z1, swap(z1, x, e1)) match {
	   *      case Abstraction(x, e1 @ Abstraction(y, e2 @ ...)) => {
	   *        Abstraction(x, Abstraction(z2, swap(z2, y e2)) match {
	   *          case Abstraction(x, e1 @ Abstraction(y, e2 @ ...)) => ...
	   *        }
	   *      }
	   *    }
	   *  }
	   * }
	   */
	  val transformedBody = {

	    /*
	     * Construct a value from the pattern and swap the bound name with
	     * the fresh one in the body of the abstraction.
	     */
	    val freshen = (boundName: Name, body: Name, freshName: Name) => {
	      val constructValueTransformer = new Transformer {
		override def transform(tree: Tree) = tree match {
		  /* Freshen the abstraction over the given bound name */
		  case pq"Abstraction($name @ ${_}, $expr)" => {
		    if (name == boundName)
		      q"Abstraction($freshName, swap($freshName, $boundName, $body))"
		    else
		      q"Abstraction($name, ${transform(expr)})"
		  }
		  /* Aliased wildcards are transformed to regular identifiers */
		  case pq"$alias @ ${Ident(nme.WILDCARD)}" => q"$alias"
		  /* In aliased expressions, the alias is removed */
		  case pq"$alias @ $expr" => q"${transform(expr)}"

		  case _ => super.transform(tree)
		}
	      }
	      constructValueTransformer.transform(transformedPattern)
	    }

	    /* Construct the (nested) match statement */
	    boundNames.foldRight(body)(
	      (name: Name, tree: Tree) => {
		val freshendValue = freshen(name, abstractions(name), freshNames(name))
		q"$freshendValue match { case $transformedPattern => $tree }"
	      }
	    )
	  }
	  
	  /* Construct a case definition with transformed pattern and body */
	  CaseDef(transformedPattern, guard, Block(freshNameDefs, transformedBody))
	}
      }
    }

    /* End of copy */

    /* ListBuffer to collect all CaseDefs from applyOrElse method */
    val caseDefs = new ListBuffer[CaseDef]() 
    val collectCaseDefsFromApply = new Transformer {
      override def transform(tree: Tree) = tree match
      {
        /* only collect from applyOrElse method */
        case x@DefDef(_,name,_,_,_,_)
           if name.toString == "applyOrElse"  => {
             collectCaseDefs.transform(x)
           }
        case _ => super.transform(tree) 
      }
      /* Collect all CaseDefs and call super transformer.
       * Has no effect on the tree itself.
       */
      val collectCaseDefs = new Transformer {
        override def transformCaseDefs(trees: List[CaseDef]) = {
          /* drop last CaseDef, because its the from compiler generated default case */
          val userDefs = trees.take(trees.size - 1)
          caseDefs ++= userDefs
          super.transformCaseDefs(trees)
        }
      }
    }
    /* Transforms Fresh.Abstraction to Abstraction,
     * because the code from FreshAnnotation expected it this way-
     */
    val removeSelectFreshAbstraction = new Transformer {
      override def transform(tree: Tree) = tree match
      {
        case Select(Ident(name),absName) 
            if name.toString == "Fresh" &&
               absName.toString == "Abstraction" => Ident(absName)
        case _ => super.transform(tree) 
      }

    }
    /* fill caseDefs with all CaseDefs from the applyOrElse method */
    collectCaseDefsFromApply.transform(patterns.tree)
    val matchStmt = removeSelectFreshAbstraction.
      transform(c.resetAllAttrs(
        q""" $expr match { case ..$caseDefs}"""))
    /* Construct definition with transformed case patterns */
    val transformedMatch = eplicitSwappingTransformer.transform(matchStmt)
    c.Expr[B](transformedMatch)
  }

}

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

import scala.util._
import java.io.{ File, FileWriter }

import Fresh._


/**
  * Generator for abstract syntax trees containing bindable names.
  */
object BenchmarkDataGenerator {

  /**
    * A type for bindable variable names
    */
  class Var


  /**
    * Abstract syntax for our object-language.
    *
    * Using bindable names allows us to define α-equivalence
    * classes of the language's abstract syntax trees.
    */
  sealed abstract class Term {

    /**
      * Fresh-ML representation of the `Term' data type.
      */
    def mlDataType: String = """ type t and var = t name;;
                                 type term = Var of var | Fn of <<var>>term | App of term * term;;"""

    /**
      * FreshML representation of this
      */
    def mlCode: String = {

      /**
        * Fresh-ML representation of an abstraction value
	*/
      def abstractionToML(abs: Abstraction[Var, Term]): (Set[Name[Var]], String) = {
	val (names, code) = termToML(abs.expr)
        (names + abs.boundName, "<<" + abs.boundName + ">> (" + code + ")")
      }

      /**
        * Fresh-ML representation of a term.
	*/
      def termToML(t: Term): (Set[Name[Var]],String) = t match {
	case Application(f, x) => {
	  val (namesInFun, codeForFun) = termToML(f)
          val (namesInArg, codeForArg) = termToML(x)
          (namesInFun ++ namesInArg, "App (" + codeForFun + ") (" + codeForArg  + ")")
	}
        case Function(abstraction) => {
          val (names, code) = abstractionToML(abstraction)
          (names, "Fn (" + code +")")
	}
        case Variable(name) => (Set(name), "Var " + name)
      }

      val (names, code) = termToML(this)
      val freshDefs = names.map(name => "let " + name + " = (fresh:var)").mkString("","in\n","in")
      mlDataType + "\n" + "let tree = " + freshDefs + "\n" + code + ";;" 
    }
  }
  case class Variable(name: Name[Var]) extends Term                      /* x */
  case class Function(function: Abstraction[Var, Term]) extends Term     /* fn x => e */
  case class Application(function: Term, argument: Term) extends Term    /* e1 e2 */



  
  /**
    * Generate a 'Term' with n abstraction values where the given variable is bound
    * in an at least one abstraction and used in the body of this abstraction.
    */
  def generate(n: Int, x: Name[Var]) : Term = {

    /**
      * Generate a 'Term' with n abstraction values where the given variable is bound
      * in an at least one abstraction and used on the right side of this abstraction.
      * 
      * @return A Tuple with the generated term, the depth of the tree, a boolean which
      * indicates that the variable is bound and used in an abstraction, and a boolean
      * which indicates that the variable is used in the body of this abstraction.
     */
    def generate(n: Int, x: Name[Var], r: Random) : (Term,Int,Boolean,Boolean) = {
      if(n == 0) {
        val z = randomName(x,r)
        (Variable(z), 1, false, x == z)
      }
      else {
        if(r.nextBoolean){
          val z = randomName(x,r)
          val (ct, cd, bound, used) = generate(n-1,x,r)
          (Function(Abstraction(z,ct)), cd +1, bound || (used && z == x), used)
        } else {
          val n2 = n.toDouble / 2
          val (lt, ld, lbound, lused) = generate(n2.ceil.toInt,x,r)
          val (rt, rd, rbound, rused) = generate(n2.floor.toInt,x,r)

          (Application(lt, rt),
            Math.max(ld,rd) +1,
            lbound || rbound,
            lused || rused)
        }
      }
    }

    /**
      * Generate a fresh bindable name.
      * 
      * @return Either a fresh name (90%) or the given one.
      */
    def randomName(name: Name[Var], rand: Random) : Name[Var] = if (rand.nextDouble >= 0.90) name
								else fresh()

    var (term, depth, bound, used) =  generate(n, x, new Random)

    /* If the given variable is not bound and used in the generated term we generate a new one */
    while(!bound) {
      val res = generate(n, x, new Random)
      term = res._1
      depth = res._2
      bound = res._3
      used = res._4
    }
    assert(bound == true && used == true)

    term
  }

  /**
    * Generate a 'Term' with the given number of abstraction values.
    */
  def generate(n: Int): Term = {

    /**
      * Generate a 'Term' with the given number of abstraction values.
      * 
      * @return The generated term and the depth of its tree.
      */
    def generate(n: Int, r: Random) : (Term,Int) = {
      if(n == 0) {
        val z : Name[Var] = fresh()
        (Variable(z), 1)
      }
      else {
        /* Create either a Abstraction or an Application */
        if(r.nextBoolean) {
          val z : Name[Var] = fresh()
          val (ct, cd) = generate(n-1,r)
          (Function(Abstraction(z,ct)), cd +1)
        } else {
          /*
	   * Half of the abstraction values are generated in the function part of
           * the application, the other half in the argument part of the application.
           */
          val n2 = n.toDouble / 2
          val (lt, ld) = generate(n2.ceil.toInt,r)
          val (rt, rd) = generate(n2.floor.toInt,r)

          (Application(lt, rt),
            Math.max(ld,rd) +1)
        }
      }
    }

    generate(n, new Random)._1
  }
  
  def writeTermToMLFile(t: Term, f : String, append:Boolean = false) = {
    val fw = new FileWriter(f, append)
    fw.write("\n" + t.mlCode + "\n")
    fw.close()
  }
}

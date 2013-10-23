
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
import scala.util._
import java.io.{ File, FileWriter }
import org.kiama.rewriting.Rewriter._

object GenerateTree {

  class Var


  /**
    * Abstract syntax for our object-language.
    *
    * Using bindable names allows us to define α-equivalence
    * classes of the language's abstract syntax trees.
    */
  sealed abstract class Term
  case class Variable(name: Name[Var]) extends Term                                              /* x */
  case class Function(function: Abstraction[Var, Term]) extends Term                             /* fn x => e */
  case class Application(function: Term, argument: Term) extends Term                            /* e1 e2 */

  val termInML = """
  type term =
   Var of var
  | Fn of <<var>>term
  | App of term * term
  """

  def toML(t: Term) : String = {
    def termToML(t:Term): (Set[Name[Var]],String) = {
      t match {
       case Application(f,x) =>
          val (lv,lt) = termToML(f)
          val (rv,rt) = termToML(x)
          (lv++rv, "App ("+lt+") (" + rt  + ")")
        case Function(a) =>
          val (v, aa) = abstractionToML(a)
          (v, "Fn (" + aa +")")
        case Variable(n) =>
          (Set(n), "Var " + n)
      }
    }

    def abstractionToML(a: Abstraction[Var, Term]) : (Set[Name[Var]],String) = {
      val (l, s) = termToML(a.expr)
      (l + a.boundName, "<<"+a.boundName+">>("+s+")")
    }
     
    val (vs, s) = termToML(t)
    val freshDefs = vs.map(x => "let "+x+ " = (fresh:var);;").mkString("","\n","")
    termInML+"\n"+freshDefs + "\n" + "val tree = "+ s + ";;" 
  }




  
  /* Generate a Term tree with n Abstractions
   * with the given variable bound in an at least on Abstraction
   * and used on the right side of the Abstraction
   */
  def generate(n:Int, x: Name[Var]) : Term = {

    /* Generate a Term tree with n Abstractions
     * with the given variable bound in an at least on Abstraction
     * and used on the right side of the Abstraction
     * 
     * @return A Tuple with the Term, the depth,
     * a boolean which indicates that the variable is bound and used in an Abstraction
     * and a boolean which indicates that the variable is used.
     */
    def generate(n: Int, x: Name[Var], r: Random) : (Term,Int,Boolean,Boolean) = {
      if(n == 0) {
        val z = randVar(x,r)
        (Variable(z), 1, false, x == z)
      }
      else {
        if(r.nextBoolean){
          val z = randVar(x,r)
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

    /* @return Either a fresh var (90%) or the given var.
     */
    def randVar(x: Name[Var], r: Random) : Name[Var] =
      if (r.nextDouble >= 0.90) x
      else fresh()

    var (term, depth, bound, used) =  generate(n, x, new Random)
    /* If the given variable is not bound and used in the generate Term
     * we generate a new Term
     */
    while(!bound)
    {
      val res = generate(n, x, new Random)
      term = res._1
      depth = res._2
      bound = res._3
      used = res._4
    }
    assert(bound == true && used == true)
    term
  }

  /* Generate a Term tree with n Abstractions
   */
  def generate(n:Int) : Term = {
    /* Generate a Term tree with n Abstractions
     * Returns the tree and its depth
     */
    def generateH(n: Int, r: Random) : (Term,Int) = {
      if(n == 0) {
        val z : Name[Var] = fresh()
        (Variable(z), 1)
      }
      else {
        /* Create either a Abstraction or an Application */
        if(r.nextBoolean) {
          val z : Name[Var] = fresh()
          val (ct, cd) = generateH(n-1,r)
          (Function(Abstraction(z,ct)), cd +1)
        } else {
          /* Half of the Abstraction is generated in the right side
           * the other half in the left side.
           */
          val n2 = n.toDouble / 2
          val (lt, ld) = generateH(n2.ceil.toInt,r)
          val (rt, rd) = generateH(n2.floor.toInt,r)

          (Application(lt, rt),
            Math.max(ld,rd) +1)
        }
      }
    }

    generateH(n, new Random)._1
  }
  
  def writeTermToMLFile(t: Term, f : String, append:Boolean = false) = {
    val fw = new FileWriter(f, append)
    fw.write("\n" + toML(t) + "\n")
    fw.close()
  }
}


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

import org.scalatest.FunSuite

import Fresh._
import ObjectLanguage._

/**
  * Tests for the algebraic support of expressions of the object-language.
  */
class StructuralEqualityTests extends FunSuite {


  /* Fresh names */
  val x: Name[Ide] = fresh()
  val y: Name[Ide] = fresh()
  val z: Name[Ide] = fresh()


  test("Simple term should be structual equal to itself") {
    val f = Fun(Abstraction(x,Var(x)))
    assert(structuralEquality(f,f))
  }

  test("Simple terms should be structual equal") {
    val f = Fun(Abstraction(z,Fun(Abstraction(y,App(Var(y),Var(z))))))
    val g = Fun(Abstraction(y,Fun(Abstraction(x,App(Var(x),Var(y))))))
    assert(structuralEquality(f,g))
  }

  test("Simple terms should not be structual equal") {
    val f = Fun(Abstraction(y,Fun(Abstraction(y,App(Var(y),Var(z))))))
    val g = Fun(Abstraction(y,Fun(Abstraction(x,App(Var(x),Var(y))))))
    assert(!structuralEquality(f,g))
  }

  test("Complex terms with list should be structual equal") {
    val f = Fun(Abstraction(z,Fun(Abstraction(y,App(Var(y),Var(z))))))
    val g = Fun(Abstraction(y,Fun(Abstraction(x,App(Var(x),Var(y))))))
    val l = Let(Abstraction(x,(Abstraction(y,Var(y)),App(Var(x),Var(z)))))
    val t1 = Tuple(List(f,l,g))
    val t2 = Tuple(List(g,l,f))
    assert(structuralEquality(t1,t2))
  }

  test("Complex terms with list should not be structural equal") {
    val f = Fun(Abstraction(y,Fun(Abstraction(y,App(Var(y),Var(z))))))
    val g = Fun(Abstraction(y,Fun(Abstraction(x,App(Var(x),Var(y))))))
    val l = Let(Abstraction(x,(Abstraction(y,Var(y)),App(Var(x),Var(z)))))
    val t1 = Tuple(List(f,l,g))
    val t2 = Tuple(List(g,l,g))
    assert(!structuralEquality(t1,t2))
  }

  test("Complex terms with different sized list should not be structural equal") {
    val f = Fun(Abstraction(y,Fun(Abstraction(y,App(Var(y),Var(z))))))
    val t1 = Tuple(List(f,f))
    val t2 = Tuple(List(f))
    assert(!structuralEquality(t1,t2))
  }
}

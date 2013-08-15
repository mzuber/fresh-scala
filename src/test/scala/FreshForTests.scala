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
  * Tests for the algebraic support of an expressions of the object-language.
  */
class FreshForTests extends FunSuite {

  /* Fresh names */
  val x: Name[Ide] = fresh()
  val y: Name[Ide] = fresh()


  test("x should be a free variable of Fun(<<y>> Var(x))") {
    assert(x.freshfor(Fun(<<(y)>> Var(x))) === false)
  }

  test("y should not be a free variable of Fun(<<y>> Var(x))") {
    assert(y.freshfor(Fun(<<(y)>> Var(x))) === true)
  }

  test("x should be a free variable of x") {
    assert(x.freshfor(x) === false)
  }

  test("x should not be a free variable of <<x>> x") {
    assert(x.freshfor(<<(x)>> x) === true)
  }

  test("x should be a free variable of <<y>> x") {
    assert(x.freshfor(<<(y)>> x) === false)
  }

  test("x should not be a free variable of <<y>> y") {
    assert(y.freshfor(<<(y)>> y) === true)
  }
}


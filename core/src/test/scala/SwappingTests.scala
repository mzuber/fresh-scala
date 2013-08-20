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
  * Test cases for swapping atoms.
  */
class SwappingTests extends FunSuite {

  /* Fresh names */
  val x: Name[Ide] = fresh()
  val y: Name[Ide] = fresh()
  val z: Name[Ide] = fresh()


  test("swapping x and y in <<x>>y should result in <<y>>x") {
    assert(swap(x, y, <<(x)>> y) === <<(y)>> x)
  }

  test("swapping x and z in <<x>>y should result in <<z>>y") {
    assert(swap(x, z, <<(x)>> y) === <<(z)>> y)
  }

  test("swapping x and y in <<z>>z should result in <<z>>z") {
    assert(swap(x, y, <<(z)>> z) === <<(z)>> z)
  }

  test("swapping x and y in <<x>>(<<y>>x) should result in <<y>>(<<x>>y)") {
    assert(swap(x, y, <<(x)>>(<<(y)>> x)) === <<(y)>>(<<(x)>> y))
  }

  test("swapping y and z in Fun(<<(y)>> Var(x)) should result in Fun(<<(z)>> Var(x))") {
    assert(swap(y, z, Fun(<<(y)>> Var(x))) === Fun(<<(z)>> Var(x)))
  }

  test("swapping [x,y] and [z,z] in App(Var(x), Var(y)) should result in App(Var(z), Var(z))") {
    assert(swap(List(x,y), List(z,z), App(Var(x), Var(y))) === App(Var(y), Var(z)))
  }
}


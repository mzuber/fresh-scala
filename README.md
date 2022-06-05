Fresh Scala
===========

Define object-level syntax modulo alpha-equivalence in Scala.

This repository contains an experiment to extend the Scala language with features that simplify metaprogramming tasks
which involve the manipulation of object language binders:
- **types of names** for representing object-level bindable names (`Name[A]`)
- **abstraction expressions** for representing object-level binding (`Abstraction[A, B]`)
- **pattern-matching** for deconstructing abstraction values (`freshMatch`, `@Fresh`, `swap`)

The project aims to replicate key aspects of the [Fresh Objective Caml](https://www.cl.cam.ac.uk/~amp12/fresh-ocaml/)
language with the goal to provide the relevant features as a library instead of a standalone language.

Installation
------------

The project can be built with [sbt](https://www.scala-sbt.org/index.html) version `0.13.18`.

Example Usage
-------------

Example use of the FreshScala system for the object-language from the
[Fresh Objective Caml User Manual](http://www.cl.cam.ac.uk/TechReports/UCAM-CL-TR-621.pdf).

```scala
/* A type for variable names */
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
case class LetFunction(letFun: Abstraction[Var, (Abstraction[Var, Term], Term)]) extends Term  /* let fun f x = e1 in e2 */
```

**Capture-avoiding substitution**: The method `subst` uses the `freshMatch` macro to compute (a representation of) the
object-level term obtained by capture-avoiding substitution of the term `e1` for all free occurrences of the variable
`x` in the term `e2`.

```scala
def subst(e1: Term, x: Name[Var], e2: Term): Term = freshMatch(e2) {
  case Variable(y) => if (x == y) e1 else Variable(y)
  case Function(Abstraction(y, e)) => Function(Abstraction(y, subst(e1, x, e)))
  case Application(f, e) => Application(subst(e1, x, f), subst(e1, x, e))
  case LetFunction(Abstraction(f, (Abstraction(y, e), body))) =>
    LetFunction(Abstraction(f, (Abstraction(y, subst(e1, x, e)), subst(e1, x, body))))
}
```

The method `substExpl` is a variation of the `subst` method which doesn't make use of abstraction patterns but performs
the freshening of the bound names explicitly.

```scala
def substExpl(e1: Term, x: Name[Var], e2: Term): Term = e2 match {
  case Variable(y) => if (x == y) e1 else Variable(y)
  case Function(Abstraction(y, e)) => {
    val z: Name[Var] = fresh() // Or: y.refresh()
    Function(<<(z)>> substExpl(e1, x, swap(z, y, e)))
  }
  case Application(f, e) => Application(substExpl(e1, x, f), substExpl(e1, x, e))
  case LetFunction(Abstraction(f, (Abstraction(y, e), body))) => {
    val g: Name[Var] = fresh() // Or: f.refresh()
    val z: Name[Var] = fresh() // Or: y.refresh()
    LetFunction(<<(g)>> swap(g, f, (<<(z)>> substExpl(e1, x, swap(z, y, e)), substExpl(e1, x, body))))
  }
}
```

The `@Fresh` marco annotation can be used define structural equality of two terms. Here the `@Fresh` annotation performs
a syntax transformation for each regular case pattern into a case pattern with explicit swapping as shown in the  `substExpl`
method.

```scala
@Fresh
def eq(e1: Term, e2: Term): Boolean = (e1, e2) match {
  case (Variable(x), Variable(y)) => x == y
  case (Function(Abstraction(x1, e1)), Function(Abstraction(x2, e2))) => eq(swap(x1, x2, e1), e2)
  case (Application(f1, e1), Application(f2, e2)) => eq(f1, f2) && eq(e1, e2)
  case (LetFunction(Abstraction(f1, (Abstraction(x1, e1), b1))), LetFunction(Abstraction(f2, (Abstraction(x2, e2), b2)))) =>
    eq(swap(f1, f2, swap(x1, x2, e1)), e2) && eq(swap(f1, f2, b1), b2)
  case _ => false
}
```



References
----------

- M.R. Shinwell and A.M. Pitts, [Fresh Objective Caml User Manual](http://www.cl.cam.ac.uk/TechReports/UCAM-CL-TR-621.pdf).
  Cambridge University Computer Laboratory, February 2005.
- M.R.Shinwell, [The Fresh Approach: functional programming with names and binders](http://www.cl.cam.ac.uk/users/amp12/fresh-ocaml/publications/shinwell-thesis.pdf).
  PhD thesis, University of Cambridge Computer Laboratory, February 2005.
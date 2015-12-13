package lx09.mandelbrot1

import org.scalatest._

import scala.util.Random._
import lx09.complex1.Complex

class Test extends FlatSpec with Matchers {

  //println(f"w: ${lx09.mandelbrot1.Model.w}")

  def rClick = Model.complex(nextInt(500), nextInt(500))

  type State = (Model.History, Int)
  def history(s: State) = s._1
  def hp(s: State) = s._2

  def D(s: State) = Model.freshImageBehavior(history(s), hp(s), rClick, rClick)
  def B(s: State) = (s._1, Model.backwardBehavior(s._1, s._2)._1)
  def F(s: State) = (s._1, Model.forwardBehavior (s._1, s._2)._1)

  def test(s: State, l: Int, d: Int) {
    val history = s._1
    val hp = s._2
    history.length should be (l)
    hp should be (d)
  }

  val s: State = (List((new Complex(-2, -2), new Complex(2, 2))), 0)
  "Initial state" should "be [_]" in {
    test(s, 1, 0)
    test(B(s), 1, 0)
    test(F(s), 1, 0)
  }

  val sD = D(s)
  "sD state" should "be [A]_" in {
    test(sD, 2, 0)
    test(F(sD), 2, 0)
    test(B(sD), 2, 1) // A[_]
  }

  val sDD = D(sD)
  "sDD state" should "be [a]A_" in {
    test(sDD,       3, 0)
    test(F(sDD),    3, 0)
    test(B(sDD),    3, 1)    // a[A]_
    test(B(B(sDD)), 3, 2)    // aA[_]
    test(B(B(B(sDD))), 3, 2) // aA[_]
  }

  val sDDB = B(sDD)
  "sDDB state" should "be a[A]_" in {
    test(sDDB, 3, 1)
    test(F(sDDB), 3, 0)    // [a]A_
    test(F(F(sDDB)), 3, 0) // [a]A_
    test(B(sDDB), 3, 2)    // aA[_]
    test(B(B(sDDB)), 3, 2) // aA[_]
  }

  val sDDBD = D(sDDB) // b[A]_
  "sDDBD state" should "be [b]A_" in {
    test(sDDBD, 3, 0)
    test(F(sDDBD), 3, 0)       // [b]A_
    test(B(sDDBD), 3, 1)       // b[A]_
    test(B(B(sDDBD)), 3, 2)    // bA[_]
    test(B(B(B(sDDBD))), 3, 2) // bA[_]
  }
}

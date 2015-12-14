package lx09.mandelbrot

import org.scalatest._

import scala.util.Random.nextInt
import lx09.complex.Complex

class Test2 extends FlatSpec with Matchers {

  import Model._

  def rClick(h: History) = complex(h, nextInt(W), nextInt(H))

  def D(h: History) = subRegion(h, rClick(h), rClick(h))
  def B(h: History) = backward(h)._1
  def F(h: History) = forward (h)._1

  def T(label: String, h: History, answer: String) {
    val size = answer.length
    (label + "'s size") should ("be " + size) in {
      h._1.length should be (size)
    }

    val index = answer.indexOf('X')
    ("The index to the current region of " + label) should ("be " + index) in {
      h._2 should be (index)
    }
  }

  val h0: History = history
  T("h0",    h0,    "X")
  T("B(h0)", B(h0), "X")
  T("F(h0)", F(h0), "X")

  val hD = D(h0)
  T("hD",    hD,    "X_")
  T("F(hD)", F(hD), "X_")
  T("B(hD)", B(hD), "_X")

  val hDD = D(hD)
  T("hDD",          hDD,          "X__")
  T("F(hDD)",       F(hDD),       "X__")
  T("B(hDD)",       B(hDD),       "_X_")
  T("B(B(hDD))",    B(B(hDD)),    "__X")
  T("B(B(B(hDD)))", B(B(B(hDD))), "__X")

  val hDDB = B(hDD)
  T("hDDB",       hDDB,       "_X_")
  T("F(hDDB)",    F(hDDB),    "X__")
  T("F(F(hDDB))", F(F(hDDB)), "X__")
  T("B(hDDB)",    B(hDDB),    "__X")
  T("B(B(hDDB))", B(B(hDDB)), "__X")

  val hDDBD = D(hDDB)
  T("hDDBD",          hDDBD,          "X__")
  T("F(hDDBD)",       F(hDDBD),       "X__")
  T("B(hDDBD)",       B(hDDBD),       "_X_")
  T("B(B(hDDBD))",    B(B(hDDBD)),    "__X")
  T("B(B(B(hDDBD)))", B(B(B(hDDBD))), "__X")
}

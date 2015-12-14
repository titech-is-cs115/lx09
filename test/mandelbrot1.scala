package lx09.mandelbrot

import org.scalatest._

import scala.util.Random.nextInt
import lx09.complex.Complex

class Test1 extends FlatSpec with Matchers {

  import Model._

  def rClick(h: History) = complex(h, nextInt(W), nextInt(H))

  def D(h: History) = subRegion(h, rClick(h), rClick(h))
  def B(h: History) = backward(h)._1
  def F(h: History) = forward (h)._1

  def T(label: String, h: History, size: Int, pos: Int) {
    (label + "'s size") should ("be " + size) in {
      h._1.length should be (size)
    }

    ("The pos to the current region of " + label) should ("be " + pos) in {
      h._2 should be (pos)
    }
  }

  val h0: History = history
  T("h0",    h0,    1, 0)
  T("B(h0)", B(h0), 1, 0)
  T("F(h0)", F(h0), 1, 0)

  val hD = D(h0)
  T("hD",    hD,    2, 0)
  T("F(hD)", F(hD), 2, 0)
  T("B(hD)", B(hD), 2, 1)

  val hDD = D(hD)
  T("hDD",          hDD,          3, 0)
  T("F(hDD)",       F(hDD),       3, 0)
  T("B(hDD)",       B(hDD),       3, 1)
  T("B(B(hDD))",    B(B(hDD)),    3, 2)
  T("B(B(B(hDD)))", B(B(B(hDD))), 3, 2)

  val hDDB = B(hDD)
  T("hDDB",       hDDB,       3, 1)
  T("F(hDDB)",    F(hDDB),    3, 0)
  T("F(F(hDDB))", F(F(hDDB)), 3, 0)
  T("B(hDDB)",    B(hDDB),    3, 2)
  T("B(B(hDDB))", B(B(hDDB)), 3, 2)

  val hDDBD = D(hDDB)
  T("hDDBD",          hDDBD,          3, 0)
  T("F(hDDBD)",       F(hDDBD),       3, 0)
  T("B(hDDBD)",       B(hDDBD),       3, 1)
  T("B(B(hDDBD))",    B(B(hDDBD)),    3, 2)
  T("B(B(B(hDDBD)))", B(B(B(hDDBD))), 3, 2)
}

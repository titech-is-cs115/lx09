package lx09.complex1

import scala.math._

class Complex(_re: Double, _im: Double) {
  def re = _re
  def im = _im

  // クラスの引数(_re, _im)を利用して定義した例
  def plus(c: Complex) = new Complex(_re + c.re, _im + c.im)

  // re, im関数を利用して定義した例
  def minus(c: Complex) = new Complex(re - c.re, im - c.im)

  def neg = new Complex(-re, -im)

  def abs = sqrt(re*re + im*im)
  
  override def toString(): String = if (im >= 0) f"($re%f+$im%fi)" else f"($re%f${im}%fi)"

  def +(c: Complex) = plus(c)
  def +(x: Double)  = new Complex(re + x, im)

  def -(c: Complex) = minus(c)
  def -(x: Double)  = new Complex(re - x, im)

  def ==(c: Complex) = re == c.re && im == c.im
  def ==(x: Double): Boolean = this==(new Complex(x, 0))

  def unary_- = neg

  def *(c: Complex) = new Complex(re * (c.re) - im * (c.im), re * (c.im) + im * (c.re))
  def *(x: Double)  = new Complex(re * x , im * x)
}

object Complex {
  def fromPolar(r: Double, theta: Double) = new Complex(r * cos(theta), r * sin(theta))
}

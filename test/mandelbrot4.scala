package lx09.mandelbrot

import org.scalatest._

import scala.util.Random.nextInt
import lx09.complex.Complex

class Test4 extends FlatSpec with Matchers {

  import Model._

  def rClick(h: History) = complex(h, nextInt(W), nextInt(H))

  type Spec = (String, History)

  def update(command: String, spec: Spec): Spec = {
    (command, spec) match {
      case ("D", (label, h)) => (f"${label}D", subRegion(h, rClick(h), rClick(h)))
      case ("B", (label, h)) => (f"${label}B", backward(h)._1)
      case ("F", (label, h)) => (f"${label}B", forward(h)._1)
    }
  }

  def T(spec: Spec, answer: String) {
    spec match { case (label, h) =>
      label should ("be " + answer) in {
        h._1.length should be (answer.length)
        h._2 should be (answer.indexOf('X'))
      }
    }
  }

  var testID = 1

  def T(specs: String, answers: String) {
    var spec = (f"$testID .", history)
    for (sa <- specs.split("").zip(answers.split(","))) {
      sa match { case (command, answer) => {
          spec = update(command, spec)
          T(spec, answer)
        }
      }
    }
    testID = testID + 1
  }

  T(("0 .", history), "X")
  T("B", "X")
  T("F", "X")

  T("DF", "X_,X_")
  T("DBB", "X_,_X,_X")

  T("DDF", "X_,X__,X__")
  T("DDBBB", "X_,X__,_X_,__X,__X")

  T("DDBFF", "X_,X__,_X_,X__,X__")
  T("DDBBB", "X_,X__,_X_,__X,__X")

  T("DDBDF",   "X_,X__,_X_,X__,X__")
  T("DDBDBBB", "X_,X__,_X_,X__,_X_,__X,__X")
}

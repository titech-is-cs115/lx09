package lx09.mandelbrot

import org.scalatest._

import scala.math.max
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
      case _ => spec
    }
  }

  def T(spec1: Spec, spec2: Spec, answer: String) {
    (spec1, spec2) match { case ((_, (h1, v1)), (label, (h2, v2))) =>
      label should ("be " + answer) in {
        // 履歴情報の大きさが仕様に合致すること
        h2.length should be (answer.length)

        // 履歴情報における表示画面位置(履歴情報の第二成分)が仕様の'[Xx]'の出現位置に合致すること
        v2 should be (max(answer.indexOf('X'), answer.indexOf('x')))

        // 履歴の底は初期画面であること
        h2(h2.length - 1) == currentRegion(history) should be (true)

        // 履歴情報に含まれる表示範囲について包含関係が成立すること．すなわち
        // 履歴情報: [ Rn, ..., R2, R1 ] のとき Rn \subset ... \subset R2 \subset R1 (= (-2-2i, 2+2i))

        for (i <- 0 to h2.length - 2) {
          (h2(i), h2(i+1)) match {
            case ((c21, c22), (c11, c12)) => {
              List(c11.re, c21.re, c22.re, c12.re) shouldBe sorted
              List(c11.im, c21.im, c22.im, c12.im) shouldBe sorted
            }
          }
        }

        // 履歴情報のうちドラッグされたばかりの領域(X)のみが変更されていること
        val scenes = answer.split("")
        val len1 = h1.length
        val len2 = h2.length
        for (s <- scenes.indices) {
          val i = len2 - 1 - s
          if (i < len1 - 1) {
            val unchanged = (h2(s) == h1(len1 - 1 - i))
            unchanged should be (scenes(s) != 'X')
          }
        }
      }
    }
  }


  var testID = 1

  def T(specs: String, answers: String) {
    var spec1 = (f"$testID .", history)
    for (sa <- specs.split("").zip(answers.split(","))) {
      sa match { case (command, answer) => {
          val spec2 = update(command, spec1)
          T(spec1, spec2, answer)
          spec1 = spec2
        }
      }
    }
    testID = testID + 1
  }

  T("B", "x")
  T("F", "x")

  T("DF", "X_,x_")
  T("DBB", "X_,_x,_x")

  T("DDF", "X_,X__,x__")
  T("DDBBB", "X_,X__,_x_,__x,__x")

  T("DDBFF", "X_,X__,_x_,x__,x__")
  T("DDBBB", "X_,X__,_x_,__x,__x")

  T("DDBDF",   "X_,X__,_x_,X__,x__")
  T("DDBDBBB", "X_,X__,_x_,X__,_x_,__x,__x")
}

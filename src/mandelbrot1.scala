package lx09.mandelbrot1

import scala.math.{abs,min,max}

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.geometry._
import scalafx.scene.{Group,Node,Scene}
import scalafx.scene.canvas.{Canvas,GraphicsContext}
import scalafx.scene.control.Button
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout.{BorderPane,HBox,Priority}
import scalafx.scene.paint.Color

import lx09.complex1.Complex

object Model {
  type Region = (Complex, Complex)

  /**
   * (w, h): 描画領域の幅と高さ
   **/
  val w = 500; val h = 500

  /**
   * history = List(c.b.a, b.a, a)
   * hisotry[hp] が表示されている．
   **/
  type History = List[Region]
  var history: History = List((new Complex(-2, -2), new Complex(2, 2)))
  var hp = 0

  var favorites: List[Region] = List()

  /**
   * complex 関数
   *
   * (x, y): (Double, Double) - 描画領域上の位置
   *
   * マウスで指定されたキャンバス上の座標(x, y)に相当する複素数を与える．
   **/
  def complex(x: Double, y: Double): Complex = {
    history(hp) match { case (c1, c2) =>
      new Complex((c1.re * (w - x) + c2.re * x) / w, (c1.im * (h - y) + c2.im * y) / h)
    }
  }

  /**
   * ドラッグを開始した点が表す複素数の値
   **/
  var p1: Complex = new Complex(0, 0)
  
  def onMousePressed(e: MouseEvent) { p1 = complex(e.x, e.y) }

  def onMouseReleased(e: MouseEvent) {
    freshImageBehavior(history, hp, p1, complex(e.x, e.y)) match {
      case (newHistory, newHp) => update(newHistory, newHp, f"${newHistory(newHp)}")
    }
  }

  def freshImageBehavior(history: History, hp: Int, p1: Complex, p2: Complex) = {
    val size = max(abs(p2.re - p1.re), abs(p2.im - p1.im))
    val c1 = new Complex((p1.re + p2.re - size) / 2, (p1.im + p2.im - size) / 2)
    val c2 = new Complex((p1.re + p2.re + size) / 2, (p1.im + p2.im + size) / 2)
    ((c1, c2) :: history.drop(hp),
      0)
  }

  def onQuit() {
    println("終了します．")
    println(f"#お気にいり = ${favorites.length}")
    for (region <- favorites) println(region)
    System.exit(0)
  }

  def update(newHistory: History, newHp: Int, logMessage: String) {
    history = newHistory
    hp = newHp
    println(f"$hp/${history.length}: $logMessage")
  }

  def backwardBehavior(history: History, hp: Int) = {
    if (hp < history.length - 1) (hp + 1, true,  f"戻る: hp = $hp")
    else                         (hp,     false, "")
  }

  def forwardBehavior(hisotry: History, hp: Int) = {
    if (hp > 0) (hp - 1, true,  f"前へ: hp = $hp")
    else        (hp,     false, "")
  }

  def theTitle() = f"Mandelbrot's microscope (${history(hp)})"

  val NCOLOR = 256
  val colors: Array[Color] =
    Array.tabulate(NCOLOR){ i =>
      Color.hsb(30, 1,
        if (i == NCOLOR - 1) 1.0
        else ((i << 2) % NCOLOR) / NCOLOR.toDouble) }

  def color(x: Int, y: Int): Color = {
    val c = complex(x, y)
    var z = new Complex(0, 0)
    var n = 0
    while (n < (NCOLOR - 1) && z.abs <= 2) {
      z = z*z + c
      n = n + 1
    }
    colors(n)
  }
}

/**
 * マンデルブロ顕微鏡の実装例
 *
 * 二つの複素数 -2+2i と 2+2i を対角頂点とする矩形複素領域について描画したのちに，ユーザがマウスドラッグによって指定する領域を拡大表示する．
 **/
object View extends JFXApp {

  val canvas = new Canvas(Model.w, Model.h)

  val quitButton = new Button("終了") { onAction = () => Model.onQuit() }
  
  canvas.onMousePressed  = (e: MouseEvent) => Model.onMousePressed(e)
  canvas.onMouseReleased = (e: MouseEvent) => Model.onMouseReleased(e)
    
  /*
  canvas.onMouseReleased = { (e: MouseEvent) =>
    freshImageBehavior(history, hp, p1, complex(e.x, e.y)) match {
      case (newHistory, newHp) => update(newHistory, newHp, f"${newHistory(newHp)}")
    }
  }
  */

  val backwardButton = new Button("戻る") { onAction = () => {
        Model.backwardBehavior(Model.history, Model.hp) match {
          case (newHp, true, logMessage) => {
            Model.update(Model.history, newHp, logMessage)
            draw()
          }
          case _ => ()
        }
      }
  }

  val forwardButton = new Button("前へ") { onAction = () => {
        Model.forwardBehavior(Model.history, Model.hp) match {
          case (newHp, true, logMessage) => {
            Model.update(Model.history, newHp, logMessage)
            draw()
          }
          case _ => ()
        }
      }
  }

  val favoriteButton = new Button("★") { onAction = () => {
        Model.favorites = Model.history(Model.hp) :: Model.favorites
        println(f"お気に入りに登録: #favorites = ${Model.favorites.length}")
      }
  }

  stage = new PrimaryStage {
    title = Model.theTitle()
    scene = new Scene {
      root = new BorderPane {
        hgrow = Priority.Always
        vgrow = Priority.Always
        center = canvas
        top = new HBox {
          children = List(quitButton, backwardButton, forwardButton, favoriteButton)
        }
      }
    }
  }

  def draw() {
    val gc = canvas.graphicsContext2D
    val w = Model.w; val h = Model.h
    gc.clearRect(0, 0, w-1, h-1)
    for (x <- Range(0, w-1)) {
      for (y <- Range(0, h-1)) {
        gc.stroke = Model.color(x, y)
        gc.strokeLine(x, y, x, y)
      }
    }
    stage.title = Model.theTitle()
  }

  draw()
}

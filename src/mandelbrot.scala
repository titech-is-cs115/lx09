package lx09.mandelbrot

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

import lx09.complex.Complex

object Model {

  val W = 500; val H = 500

  type Region = (Complex, Complex)

  type History = (List[Region], Int)
  var history = {
    val c1 = new Complex(-2, -2); val c2 = new Complex(2, 2);
    (List((c1, c2)), 0)
  }

  def currentRegion(h: History) = h._1(h._2)
  def dropToCurrent(h: History) = h._1.drop(h._2)

  var favorites: List[Region] = Nil

  def complex(h: History, x: Double, y: Double): Complex = {
    currentRegion(h) match { case (c1, c2) =>
      new Complex((c1.re * (W - x) + c2.re * x) / W, (c1.im * (H - y) + c2.im * y) / H)
    }
  }

  var p1: Complex = new Complex(0, 0)
  
  def onDragStart (e: MouseEvent) { p1 = complex(history, e.x, e.y) }
  def onDragFinish(e: MouseEvent) { update(subRegion(history, p1, complex(history, e.x, e.y))) }

  def subRegion(h: History, p1: Complex, p2: Complex) = {
    val size = max(abs(p2.re - p1.re), abs(p2.im - p1.im))
    val c1 = new Complex((p1.re + p2.re - size) / 2, (p1.im + p2.im - size) / 2)
    val c2 = new Complex((p1.re + p2.re + size) / 2, (p1.im + p2.im + size) / 2)
    ((c1, c2) :: dropToCurrent(h), 0)
  }

  def onQuit() {
    println("終了します．")
    println(f"#お気にいり = ${favorites.length}")
    for (region <- favorites) println(region)
    System.exit(0)
  }

  def update(h: History) { history = h }

  def backward(h: History) = {
    if (h._2 < h._1.length - 1) ((h._1, h._2 + 1), true)
    else                         (h,               false)
  }

  def forward(h: History) = {
    if (h._2 > 0) ((h._1, h._2 - 1), true)
    else           (h,               false)
  }

  def theTitle() = f"Mandelbrot's microscope (${currentRegion(history)})"

  val NCOLOR = 256
  val colors: Array[Color] =
    Array.tabulate(NCOLOR){ i =>
      Color.hsb(30, 1,
        if (i == NCOLOR - 1) 1.0
        else ((i << 2) % NCOLOR) / NCOLOR.toDouble) }

  def color(x: Int, y: Int): Color = {
    val c = complex(history, x, y)
    var z = new Complex(0, 0)
    var n = 0
    while (n < (NCOLOR - 1) && z.abs <= 2) {
      z = z * z + c
      n = n + 1
    }
    colors(n)
  }
}

object View extends JFXApp {

  import Model._

  val canvas = new Canvas(W, H) {
    onMousePressed  = (e: MouseEvent) => onDragStart(e)
    onMouseReleased = (e: MouseEvent) => { onDragFinish(e); draw() }
  }

  val quitButton = new Button("終了") { onAction = () => onQuit() }

  val backwardButton = new Button("戻る") {
    onAction = () => {
      backward(history) match {
        case (h, true) => { update(h); draw() }
        case _ => ()
      }
    }
  }

  val forwardButton = new Button("前へ") {
    onAction = () => {
      forward(history) match {
        case (h, true) => { update(h); draw() }
        case _ => ()
      }
    }
  }

  val favoriteButton = new Button("★") { onAction = () => { favorites = currentRegion(history) :: favorites } }

  stage = new PrimaryStage {
    title = theTitle()
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
    val pw = gc.pixelWriter
    gc.clearRect(0, 0, W-1, H-1)
    for (x <- Range(0, W-1); y <- Range(0, H-1))
      pw.setColor(x, y, color(x, y))
    stage.title = theTitle()
  }

  draw()
}

import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.control.Button
import scalafx.scene.image.Image

import javafx.embed.swing
import scalafx.scene.image.{Image, WritableImage, ImageView}

import java.awt.image.BufferedImage

import io.StdIn._

case class LightSource(point: Vect3)
// Input functions

class Main() {
  def getThreeValues(prompt: String): Vect3 = {
    print(prompt + " ")
    val ln = readLine
    val dataStr: Array[String] = ln.split(" ")

    if (dataStr.length != 3) {
        println("Y are you so bad at life")
        println(dataStr(0) +" "+ dataStr(1) +" "+ dataStr(2))
        return getThreeValues(prompt)
    }

    try {
        val nums: Array[Double] = dataStr.map(_.trim.toDouble)
        return new Vect3(nums(0), nums(1), nums(2))
    }
    catch {
        case x: NumberFormatException => {
            println("Y are you so bad at life")
            println(dataStr(0) +" "+ dataStr(1) +" "+ dataStr(2))
        }
    }
    return getThreeValues(prompt)
  }

  def getOneValue(prompt: String): Double = {
    print(prompt + ": ")
    val ln = readLine

    try {
        return ln.trim.toDouble
    }
    catch {
        case x: NumberFormatException => {
            println("Y are you so bad at life")
            println(ln)
        }
    }
    return getOneValue(prompt)
  }

  // case classes


  def main1(args: Array[String]): Unit = {
    println("Start2")

    val side: Int = 300

    val up: Vect3 = getThreeValues("Up Vector")
    val forward: Vect3 = getThreeValues("Forward Vector")

    val objects: Array[GeometricObject] = Array(
        new Sphere(new Vect3(0,0,0), 20, new DColor(255, 0, 0), 0),
        new Sphere(new Vect3(20,40,20), 20, new DColor(255, 0, 0), 0),
        new Sphere(new Vect3(0,-40,0), 10, new DColor(255, 0, 0), 0),
        new Plane(new Vect3(0, -0.3, -1), 40, new DColor(0, 255, 0), 0.1)
    )

    val lsrc = LightSource(new Vect3(0, 0, 100))

    val grid = new Grid(new Vect3(100, 0, 0), forward, up, side)

    val img = new BufferedImage(side, side, BufferedImage.TYPE_INT_RGB)

    println(s"${side}, ${side}")

    for (i <- 0 until side; j <- 0 until side) {
        val c: DColor = grid.rayTraceOnce(objects, lsrc, i, j)

        img.setRGB(i, j, c.assembleRGB())
    }

    val w: WritableImage = new WritableImage(side, side)

    val iv: ImageView = new ImageView(w)

    val p = swing.SwingFXUtils.toFXImage(img, w)

    val app = new JFXApp {
        stage = new JFXApp.PrimaryStage {
            title = "Ray Tracing 3"
            scene = new Scene(side.toDouble, side.toDouble) {
                //fill = Color.Coral
                //val button = new Button("Click me!")
                content = iv
            }
        }
    }

    app.main(args)

  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val m = new Main()
    m.main1(args)
  }
}

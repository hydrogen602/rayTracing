package rayTracing

import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.control.Button
import scalafx.scene.image.Image

import javax.imageio.ImageIO
import java.io.File

import javafx.embed.swing
import scalafx.scene.image.{Image, WritableImage, ImageView}

import java.awt.image.BufferedImage

import io.StdIn._

import data._
import geometricObject._
import data.LightSource

// Input functions

object Main {

  val config = data.Config("config.xml")

  def stringToVect(s: String): Vect3 = {
    val Array(a, b, c) = s.split(",")
    Vect3(a.toDouble, b.toDouble, c.toDouble)
  }

  def stringToDColor(s: String): DColor = {
    val Array(a, b, c) = s.split(",")
    DColor(a.toDouble, b.toDouble, c.toDouble)
  }

  def main(args: Array[String]) {
    println("Start2")

    def generateSphere(n: xml.Node): Sphere = {
      val center: Vect3 = stringToVect((n \ "center").text)
      val radius: Double = (n \ "radius").text.toDouble
      val color: DColor = stringToDColor((n \ "color").text)
      val refl: Double = (n \ "reflectivity").text.toDouble
      require(0 <= refl && refl <= 1, "Reflectivity must be in range [0, 1]")

      new Sphere(center, radius, color, refl)
    }

    def generatePlane(n: xml.Node): Plane = {
      val normal: Vect3 = stringToVect((n \ "normal").text)
      val d: Double = (n \ "distance").text.toDouble
      val color: DColor = stringToDColor((n \ "color").text)
      val refl: Double = (n \ "reflectivity").text.toDouble
      require(0 <= refl && refl <= 1, "Reflectivity must be in range [0, 1]")

      new Plane(normal, d, color, refl)
    }

    val xmlObjects = xml.XML.loadFile("objects.xml")
    val spheres = (xmlObjects \ "sphere").map(generateSphere).toArray
    val planes = (xmlObjects \ "plane").map(generatePlane).toArray

    val objects: Array[GeometricObject] = spheres ++ planes

    val lsrc = LightSource(Vect3(0, 0, 100), DColor(255, 255, 255))

    val grid = new Grid(config.raySource, config.forward, config.up, config.side)

    val img = new BufferedImage(config.side, config.side, BufferedImage.TYPE_INT_RGB)

    println(s"${config.side}, ${config.side}")

    for (i <- 0 until config.side; j <- 0 until config.side) {
        val c: DColor = grid.rayTraceOnce(objects, lsrc, i, j)

        img.setRGB(i, j, c.assembleRGB())
    }

    val w: WritableImage = new WritableImage(config.side, config.side)

    val iv: ImageView = new ImageView(w)

    val p = swing.SwingFXUtils.toFXImage(img, w)

    //val file: java.io.File = new File("test.png")

    //ImageIO.write(img, "png", file)

    val app = new JFXApp {
        stage = new JFXApp.PrimaryStage {
            title = "Ray Tracing 3"
            scene = new Scene(config.side.toDouble, config.side.toDouble) {
                content = iv
            }
        }
    }

    app.main(args)

  }
}

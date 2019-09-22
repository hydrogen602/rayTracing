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

object Main {

  def stringToVect(s: String): Vect3 = {
    val Array(a, b, c) = s.split(",")
    Vect3(a.toDouble, b.toDouble, c.toDouble)
  }

  def stringToDColor(s: String): DColor = {
    val Array(a, b, c) = s.split(",")
    DColor(a.toDouble, b.toDouble, c.toDouble)
  }

  def main(args: Array[String]): Unit = {
    println("Start2")

    val config = xml.XML.loadFile("config.xml")

    val side: Int = (config \ "side").text.toInt

    val up: Vect3 = stringToVect((config \ "upVector").text)
    val forward: Vect3 = stringToVect((config \ "forwardVector").text)
    val raySource: Vect3 = stringToVect((config \ "raySource").text)

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

    val lsrc = LightSource(Vect3(0, 0, 100))

    val grid = new Grid(raySource, forward, up, side)

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

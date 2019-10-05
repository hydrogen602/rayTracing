package rayTracing

import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.control.Button
import scalafx.scene.image.Image
import scalafx.animation.AnimationTimer

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

  var lastT: Double = -1
  var totalSec: Double = 0

  def stringToVect(s: String): Vect3 = {
    val Array(a, b, c) = s.split(",")
    Vect3(a.toDouble, b.toDouble, c.toDouble)
  }

  def stringToDColor(s: String): DColor = {
    val Array(a, b, c) = s.split(",")
    DColor(a.toDouble, b.toDouble, c.toDouble)
  }

  def generateImage(grid: Grid, img: BufferedImage, objects: Array[GeometricObject]) {
    for (i <- 0 until config.side; j <- (0 until config.side).par) {
      val colors: List[DColor] = for (lsrc <- config.lightSources) yield {
        grid.rayTraceOnce(objects, lsrc, i, j)
      }
      val c = colors.reduce((a, b) => (a + b)/2)

      img.setRGB(i, j, c.assembleRGB())
    }
  }

  def spin(theta: Double): (Vect3, Vect3, Vect3) = {
    // spin around the z-axis
    // theta = 0 is <200, 0, 0>
    // theta = pi/2 is <0, 200, 0>
    // theta = pi is <-200, 0, 0>
    val horizontalDistance = config.raySource.mag
    val x = math.cos(theta) * horizontalDistance
    val y = math.sin(theta) * horizontalDistance
    val raySource = Vect3(x, y, config.raySource.z)

    val forwardVect = (-raySource).normalize
    val upVect = Vect3(0, 0, 1)
    (raySource, forwardVect, upVect)
  }

  def renderImage(grid: Grid, objects: Array[GeometricObject]): ImageView = {
    val img = new BufferedImage(config.side, config.side, BufferedImage.TYPE_INT_RGB)

    generateImage(grid, img, objects)

    val w: WritableImage = new WritableImage(config.side, config.side)

    val iv: ImageView = new ImageView(w)

    val p = swing.SwingFXUtils.toFXImage(img, w)

    //val file: java.io.File = new File("test.png")

    //ImageIO.write(img, "png", file)
    return iv
  }

  def loadObjects(xmlFileName: String): Array[GeometricObject] = {
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

    val xmlObjects = xml.XML.loadFile(xmlFileName)
    val spheres = (xmlObjects \ "sphere").map(generateSphere).toArray
    val planes = (xmlObjects \ "plane").map(generatePlane).toArray
    spheres ++ planes
  }

  def main(args: Array[String]) {
    println("Start2")

    val objects: Array[GeometricObject] = loadObjects("objects.xml")

    println(s"${config.side}, ${config.side}")

    val app = new JFXApp {
        stage = new JFXApp.PrimaryStage {
            title = "Ray Tracing 3"
            scene = new Scene(config.side.toDouble, config.side.toDouble) {

                def stop() {
                  timer.stop()
                }
          
                val timer = AnimationTimer(time => {
                  val deltaT: Double = if (lastT > 0) {
                    val diff = time - lastT
                    lastT = time
                    diff
                  }
                  else {
                    lastT = time
                    0
                  }
                  val deltaSec = deltaT / 1000000000 // 10^9
                  totalSec += deltaSec
                  val (raySrc, forwardVect, upVect) = spin(totalSec / 10)
                  //assert(forwardVect.squareOfMag() == 1)
                  val grid = new Grid(raySrc, forwardVect, upVect, config.side)

                  content = renderImage(grid, objects)
                })
                timer.start()
            }
        }
    }

    app.main(args)

  }
}

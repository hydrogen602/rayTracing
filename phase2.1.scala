import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.control.Button
import scalafx.scene.image.Image

import javafx.embed.swing
import scalafx.scene.image.{Image, WritableImage, ImageView}

import java.awt.image.BufferedImage

import io.StdIn._


// Data structures

// Vectors
case class Vect3(x: Double, y: Double, z: Double)

// Geometric Objects
abstract class Data
case class Plane(vect: Vect3, d: Double, color: DColor) extends Data
case class Sphere(vect: Vect3, d: Double, color: DColor) extends Data

// Collection of Objects
case class SceneObjects(objects: List[Data])

case class DColor(r: Double, g: Double, b: Double)
// Light ray with color
case class Ray(src: Vect3, dir: Vect3, color: DColor)


// Input functions

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
        return Vect3(nums(0), nums(1), nums(2))
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

def intersectionWithSphere(r: Ray, sp: Data): Double = {
    /* 
     * r0   =   start point of ray
     * r    =   direction from start
     * n    =   center of the sphere
     * 
     * returns scalar t that satisfies |r(t) - n|^2 = radius^2
     * where r(t) is r0 + r * t
     *
     * returns -1 upon failure aka no solution
     */

    // r0 + r * t is the point of the tip of the ray

    // see CS notebook for math
    val (n, radius, color) = sp match {
        case Sphere(vect, d, color) => (vect, d, color)
        case _ => return -1
    }

    // components of quadratic equation
    // ax^2 + bx + c = 0
    val a = squareOfMag(r.dir)
    val b = -2 * dot(n, r.dir) + 2 * dot(r.src, r.dir)
    val c = squareOfMag(n) - 2 * dot(n, r.src) + squareOfMag(r.src) - radius * radius

    // quadratic equation time

    // -b +- sqrt(b^2 - 4ac)
    // ---------------------
    //          2a

    val thingInSquareRoot = b * b - 4 * a * c
    //println(thingInSquareRoot)

    if (thingInSquareRoot < 0) {
        // no solution
        return -1 
    }

    val positiveSide = (-b + math.sqrt(thingInSquareRoot)) / (2 * a)
    val negativeSide = (-b - math.sqrt(thingInSquareRoot)) / (2 * a)

    //println("solutions = " + positiveSide + " and " + negativeSide)

    return if (positiveSide < 0 && negativeSide < 0) -1
        else if (positiveSide < 0) negativeSide
        else if (negativeSide < 0) positiveSide
        else if (positiveSide < negativeSide) positiveSide
        else negativeSide
}

def intersectionWithPlane(r: Ray, pl: Data): Double = {
    /* 
     * r0   =   start point of ray
     * r =   direction from start
     * n    =   normal List from origin to plane
     * 
     * returns scalar t that satisfies r(t) * n = d
     * where r(t) is r0 + r * t
     *
     */

     // see CS notebook for math
     val (n, d, color) = pl match {
        case Plane(vect, d, color) => (vect, d, color)
        case _ => return -1
    }

     return (d - dot(n, r.src)) / dot(n, r.dir)
}

def intersection(r: Ray, sc: SceneObjects): (Double, Data) = {

    def intersectionFunc(dat: Data): Double = {
        return dat match {
            case Sphere(_, _, _) => intersectionWithSphere(r, dat)
            case Plane(_, _, _) => intersectionWithPlane(r, dat)
        }
    }

    val solutionExists = (x: (Double, Data)) => x._1 >= 0 // funcs return -1 on failure, this weeds that out

    val distances: List[(Double, Data)] = sc.objects map intersectionFunc zip sc.objects filter solutionExists //min

    val minFunc = (a: (Double, Data), b: (Double, Data)) => if (a._1 < b._1) a else b

    return if (distances.length > 0) distances.reduce(minFunc) else (-1, null)
}

def scale(v: Vect3, t: Double): Vect3 = Vect3(v.x * t, v.y * t, v.z * t)

def add(a: Vect3, b: Vect3): Vect3 = Vect3(a.x + b.x, a.y + b.y, a.z + b.z)

def sub(a: Vect3, b: Vect3): Vect3 = Vect3(a.x - b.x, a.y - b.y, a.z - b.z)

def neg(a: Vect3): Vect3 = Vect3(-a.x, -a.y, -a.z)

def cross(a: Vect3, b: Vect3): Vect3 = Vect3(a.y * b.z - a.z * b.y, -(a.x * b.z - a.z * b.x), a.x * b.y - a.y * b.x)

def dot(a: Vect3, b: Vect3): Double = a.x * b.x + a.y * b.y + a.z * b.z

def squareOfMag(a: Vect3): Double = a * a

def unitVector(v: Vect3): Vect3 = {
    val mag = math.sqrt(squareOfMag(v))

    return Vect3(v.x / mag, v.y / mag, v.z / mag)
}

def rayTrace(raySource: Vect3, forward: Vect3, up: Vect3, sc: SceneObjects, heightOrWidthPixel: Int): Array[Array[Double]] = {
    // forward and up should be unit vectors
    if (squareOfMag(forward) != 1 || squareOfMag(up) != 1) {
        throw new Exception("Forward and Up must be unit vectors")
    }

    val left: Vect3 = cross(up, forward)

    val centerOfPixelGrid = add(raySource, forward) // forward should be a unit vector

    val topLeftCorner = add(add(centerOfPixelGrid, up), left)
    val bottomRightCorner = sub(centerOfPixelGrid, add(left, up))

    println(s"${topLeftCorner.x}, ${topLeftCorner.y}, ${topLeftCorner.z}")
    println(s"${bottomRightCorner.x}, ${bottomRightCorner.y}, ${bottomRightCorner.z}")

    val scalar = heightOrWidthPixel - 1
    /*
     * Think of a grid. It has squares, but for simplicity I'm going to shoot the rays
     * at the corners of the box. Now there is one less square lengthwise than there
     * are corners between the squares, and so that is why I'm subtracting one here.
     * 
     *   X--X--X   Note: There are 3 corners (X) across, but 2 line segments (--)
     *   |  |  |     So I need to divide the side of the square grid by 2
     *   X--X--X     instead of 3 to get the distance between corners.
     *   |  |  | 
     *   X--X--X 
     */

    val rightStepVector = scale(scale(neg(left), 2), 1.0 / scalar)
    val downStepVector = scale(scale(neg(up), 2), 1.0 / scalar)
    // these can be combined with the top left corner to get any corner of the grid

    // first will be up/down, second left/right

    val assembleRayDir = (i: Int, j: Int) => {
        add(
            scale(rightStepVector, j), 
            scale(downStepVector, i)
        ), 
    }

    val allValuesT = Array.ofDim[Double](heightOrWidthPixel, heightOrWidthPixel)

    for (i <- 0 until heightOrWidthPixel; j <- 0 until heightOrWidthPixel) {

        val point = add(assembleRayDir(i, j), topLeftCorner)

        val ray: Ray = Ray(raySource, unitVector(sub(point, raySource)), DColor(255, 255, 255))

        val (t, obj) = intersection(ray, sc)

        allValuesT(i)(j) = t

        //println(s"i = $i, j = $j")
        //println(s"${point.x}, ${point.y}, ${point.z}")
    }

    val round1 = (d: Double) => (d*10+0.5).toInt / 10.0
    // round1 ONLY works for postive numbers!!!

    // for (i <- 0 until heightOrWidthPixel) {
    //     for (j <- 0 until heightOrWidthPixel) {
    //         val d: Double = round1(allValuesT(i)(j))
    //         val s: String = if (d < 0) "X" else d.toString
    //         print(s"$s\t")
    //     }
    //     println()
    // }
    return allValuesT
}

def main(): Unit = {
    val side: Int = 300

    val up: Vect3 = getThreeValues("Up Vector")
    val forward: Vect3 = getThreeValues("Forward Vector")

    val sc: SceneObjects = SceneObjects(List(
        Sphere(Vect3(0,0,0), 70, DColor(255, 0, 0)), 
        Plane(Vect3(1, 0, 3), 0, DColor(0, 255, 0))
    ))

    val allT: Array[Array[Double]] = rayTrace(Vect3(100, 0, 0), forward, up, sc, side)

    for (i <- 0 until side) {
        for (j <- 0 until side) {
            //print(s"${(allT(i)(j)).toInt}\t")
        }

        //println()
    }


    val img = new BufferedImage(side, side, BufferedImage.TYPE_INT_RGB)

    val disToColor = (d: Double) => 3 * d

    val fixNeg = (d: Double) => if (d < 0) 0 else if (d > 255) 255 else d.toInt


    val assembleRGB = (c: DColor) => c.r.toInt << 16 | c.g.toInt << 8 | c.b.toInt

    println(s"${allT.size}, ${allT(0).size}")

    for (i <- 0 until side; j <- 0 until side) {
        val c: DColor = DColor(fixNeg(disToColor(allT(i)(j))), 0, 0)
        img.setRGB(i, j, assembleRGB(c))
    }

    val w: WritableImage = new WritableImage(side, side)

    val iv: ImageView = new ImageView(w)

    val p = swing.SwingFXUtils.toFXImage(img, w)

    val app = new JFXApp {
        stage = new JFXApp.PrimaryStage {
            title = "Ray Tracing 2.1"
            scene = new Scene(side.toDouble, side.toDouble) {
                //fill = Color.Coral
                //val button = new Button("Click me!")
                content = iv
            }
        }
    }

    app.main(args)

}

main()





import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.control.Button
import scalafx.scene.image.Image

import javafx.embed.swing
import scalafx.scene.image.{Image, WritableImage, ImageView}

import java.awt.image.BufferedImage

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

class DColor(rArg: Double, gArg: Double, bArg: Double) {

    val r = rArg
    val g = gArg
    val b = bArg

    def assembleRGB(): Double = {
        require(r < 0 || r > 255 || g < 0 || g > 255 || b < 0 || b > 255 , "rgb values not in range [0, 255]")
        r.toInt << 16 | g.toInt << 8 | b.toInt
    }

}

class Vect3(xArg: Double, yArg: Double, zArg: Double) {

    val x: Double = xArg
    val y: Double = yArg
    val z: Double = zArg

    def apply(index: Int): Double = {
        index match {
            case 0 => x
            case 1 => y
            case 2 => z
            case _ => throw new IndexOutOfBoundsException(s"$index")
        }
    }

    def +(other: Vect3): Vect3 = new Vect3(x + other.x, y + other.y, z + other.z)

    def -(other: Vect3): Vect3 = new Vect3(x - other.x, y - other.y, z - other.z)

    def *(sc: Double): Vect3 = new Vect3(x * sc, y * sc, z * sc)

    def /(sc: Double): Vect3 = new Vect3(x / sc, y / sc, z / sc)

    // dot product
    def *(other: Vect3): Double = x * other.x + y * other.y + z * other.z

    // cross product
    def x(v: Vect3): Vect3 = new Vect3(y * v.z - z * v.y, -(x * v.z - z * v.x), x * v.y - y * v.x)

    def squareOfMag(): Double = this * this

    def mag(): Double = math.sqrt(this * this)

    def normalize(): Vect3 = this / mag() 

    def unary_+(): Vect3 = new Vect3(x, y, z)

    def unary_-(): Vect3 = new Vect3(-x, -y, -z)

    override def toString(): String = s"<$x, $y, $z>"
}

abstract class GeometricObject {
    val n: Vect3
    val d: Double
    val c: DColor
    def intersection(r: Ray): Double
}

class Plane(normal: Vect3, dArg: Double, color: DColor) extends GeometricObject {
    val n: Vect3 = normal
    val d: Double = dArg
    val c: DColor = color

    def intersection(r: Ray): Double = {
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

         val tmp = (d - (n * r.src)) / (n * r.dir)
         return if (tmp == Double.PositiveInfinity || tmp == Double.NegativeInfinity) -1 else tmp
    }

    override def toString(): String = "Plane"
}

class Sphere(centerArg: Vect3, radiusArg: Double, color: DColor) extends GeometricObject {
    val n: Vect3 = centerArg
    val d: Double = radiusArg
    val c: DColor = color

    def intersection(r: Ray): Double = {
        /* 
         * r0   =   start point of ray
         * r    =   direction from start
         * n    =   center of the sphere
         * d    =   radius of sphere
         * 
         * returns scalar t that satisfies |r(t) - n|^2 = radius^2
         * where r(t) is r0 + r * t
         *
         * returns -1 upon failure aka no solution
         */

        // r0 + r * t is the point of the tip of the ray

        // see CS notebook for math

        // components of quadratic equation
        // ax^2 + bx + c = 0
        val a = r.dir.squareOfMag
        val b = -2 * (n * r.dir) + 2 * (r.src * r.dir)
        val c = n.squareOfMag - 2 * (n * r.src) + r.src.squareOfMag - d * d

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

    override def toString(): String = "Sphere"
}

class Grid(raySource: Vect3, forwardArg: Vect3, upArg: Vect3, sideArg: Int) {
    require(forwardArg.squareOfMag == 1 && upArg.squareOfMag == 1)

    val src: Vect3 = raySource
    val forward: Vect3 = forwardArg
    val up: Vect3 = upArg
    val left: Vect3 = up x forward

    val side: Int = sideArg

    val centerOfPixelGrid = raySource + forward

    val topLeftCorner = centerOfPixelGrid + up + left
    val bottomRightCorner = centerOfPixelGrid - up - left

    val scalar = side - 1
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

    val rightStepVector = (-left * 2) / scalar
    val downStepVector = (-up * 2) / scalar

    def getPoint(i: Int, j: Int): Vect3 = {
        // returns the x, y, z of point (i, j) in the grid
        topLeftCorner + (rightStepVector * j) + (downStepVector * i)
    }

    def rayTrace(objects: Array[GeometricObject]): Array[Array[Double]] = {
        val allValuesT = Array.ofDim[Double](side, side)

        for (i <- 0 until side; j <- 0 until side) {

            val point = getPoint(i, j)

            val ray = new Ray(src, point - src, new DColor(0, 0, 0))

            val (t, obj) = ray.trace(objects)

            allValuesT(i)(j) = t

            //println(s"i = $i, j = $j")
            //println(s"${point.x}, ${point.y}, ${point.z}")
        }

        return allValuesT
    }
}

class Ray(srcArg: Vect3, dirArg: Vect3, color: DColor) {
    val src: Vect3 = srcArg
    val dir: Vect3 = dirArg.normalize
    val c: DColor = color

    def trace(objects: Array[GeometricObject]): (Double, GeometricObject) = {

        def intersectionFunc(geometry: GeometricObject): Double = {
            geometry.intersection(this)
        }

        //val solutionExists = (x: (Double, GeometricObject)) => x._1 >= 0 // funcs return -1 on failure, this weeds that out

        val distances: Array[(Double, GeometricObject)] = objects map intersectionFunc zip objects filter (_._1 >= 0) // solutionExists

        val minFunc = (a: (Double, GeometricObject), b: (Double, GeometricObject)) => if (a._1 < b._1) a else b

        return if (distances.length > 0) distances.reduce(minFunc) else (-1, null)
    }
}

// custom interators(?),


def main(): Unit = {
    val side: Int = 300

    val up: Vect3 = getThreeValues("Up Vector")
    val forward: Vect3 = getThreeValues("Forward Vector")

    val objects: Array[GeometricObject] = Array(
        Sphere(Vect3(0,0,0), 70, new DColor(255, 0, 0)), 
        Plane(Vect3(1, 0, 3), 0, new DColor(0, 255, 0))
    )

    val grid = new Grid(new Vect3(100, 0, 0), forward, up, side)

    val allT: Array[Array[Double]] = grid.rayTrace(objects)

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
        val c: DColor = new DColor(fixNeg(disToColor(allT(i)(j))), 0, 0)
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
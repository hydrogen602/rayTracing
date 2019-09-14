
import io.StdIn._

case class Vect3(x: Double, y: Double, z: Double)

abstract class Data
case class Plane(vect: Vect3, d: Double) extends Data
case class Sphere(vect: Vect3, d: Double) extends Data

case class Scene(objects: List[Data])

// p. 397

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
        val a: Double = dataStr(0).trim.toDouble
        val b: Double = dataStr(1).trim.toDouble
        val c: Double = dataStr(2).trim.toDouble
        return Vect3(a, b, c)
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

def intersectionWithSphere(r0: Vect3, r: Vect3, sp: Data): Double = {
    /* 
     * r0   =   start point of ray
     * r =   direction from start
     * n    =   center of the sphere
     * 
     * returns scalar t that satisfies |r(t) - n|^2 = radius^2
     * where r(t) is r0 + r * t
     *
     * returns -1 upon failure aka no solution
     */

    // r0 + r * t is the point of the tip of the ray

    // see CS notebook for math
    val (n, radius) = sp match {
        case Sphere(vect, d) => (vect, d)
        case _ => return -1
    }

    // components of quadratic equation
    // ax^2 + bx + c = 0
    val a = squareOfMag(r)
    val b = -2 * dot(n, r) + 2 * dot(r0, r)
    val c = squareOfMag(n) - 2 * dot(n, r0) + squareOfMag(r0) - radius * radius

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

def intersectionWithPlane(r0: Vect3, r: Vect3, pl: Data): Double = {
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
     val (n, d) = pl match {
        case Plane(vect, d) => (vect, d)
        case _ => return -1
    }

     return (d - dot(n, r0)) / dot(n, r)
}

def intersection(r0: Vect3, r: Vect3, sc: Scene): (Double, Data) = {

    def intersectionFunc(dat: Data): Double = {
        return dat match {
            case Sphere(_, _) => intersectionWithSphere(r0, r, dat)
            case Plane(_, _) => intersectionWithPlane(r0, r, dat)
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

def squareOfMag(a: Vect3): Double = dot(a, a)

def unitVector(v: Vect3): Vect3 = {
    val mag = math.sqrt(squareOfMag(v))

    return Vect3(v.x / mag, v.y / mag, v.z / mag)
}

def rayTrace(raySource: Vect3, forward: Vect3, up: Vect3, sc: Scene, heightOrWidthPixel: Int): Unit = {
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

    //               vector along edge of grid / num of pixels
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

        val rayDir = unitVector(sub(point, raySource))

        val (t, obj) = intersection(raySource, rayDir, sc)

        allValuesT(i)(j) = t

        //println(s"i = $i, j = $j")
        //println(s"${point.x}, ${point.y}, ${point.z}")
    }

    val round1 = (d: Double) => (d*10+0.5).toInt / 10.0
    // round1 ONLY works for postive numbers!!!

    for (i <- 0 until heightOrWidthPixel) {
        for (j <- 0 until heightOrWidthPixel) {
            val d: Double = round1(allValuesT(i)(j))
            val s: String = if (d < 0) "X" else d.toString
            print(s"$s\t")
        }
        println()
    }
    
}

def main(): Unit = {
    val up: Vect3 = getThreeValues("Up Vector")
    val forward: Vect3 = getThreeValues("Forward Vector")

    val sc: Scene = Scene(List(Sphere(Vect3(0,0,0), 6)))

    rayTrace(Vect3(10, 0, 0), forward, up, sc, 15)
}

main()

























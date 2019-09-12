
import io.StdIn._

case class Vect3(x: Double, y: Double, z: Double)

abstract class Data
case class Plane(vect: Vect3, d: Double) extends Data
case class Sphere(vect: Vect3, d: Double) extends Data


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

def scale(v: Vect3, t: Double): Vect3 = Vect3(v.x * t, v.y * t, v.z * t)

def addition(a: Vect3, b: Vect3): Vect3 = Vect3(a.x + b.x, a.y + b.y, a.z + b.z)

def cross(a: Vect3, b: Vect3): Vect3 = Vect3(a.y * b.z - a.z * b.y, -(a.x * b.z - a.z * b.x), a.x * b.y - a.y * b.x)

def dot(a: Vect3, b: Vect3): Double = a.x * b.x + a.y * b.y + a.z * b.z

def squareOfMag(a: Vect3): Double = dot(a, a)

def unitVector(v: Vect3): Vect3 = {
    val mag = math.sqrt(squareOfMag(v))

    return Vect3(v.x / mag, v.y / mag, v.z / mag)
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

    println("solutions = " + positiveSide + " and " + negativeSide)

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

def intersection(r0: Vect3, r: Vect3, objects: Array[Data]): (Double, Data) = {

    def intersectionFunc(dat: Data): Double = {
        return dat match {
            case Sphere(_, _) => intersectionWithSphere(r0, r, dat)
            case Plane(_, _) => intersectionWithPlane(r0, r, dat)
        }
    }

    val solutionExists = (x: (Double, Data)) => x._1 >= 0 // funcs return -1 on failure, this weeds that out

    val distances: Array[(Double, Data)] = objects map intersectionFunc zip objects filter solutionExists //min

    def compareFunc(it: Iterator[(Double, Data)], lowestDis: (Double, Data)): (Double, Data) = {
        if (it.hasNext) {
            val next: (Double, Data) = it.next()
            if (next._1 < lowestDis._1) {
                return compareFunc(it, next)
            }
            return compareFunc(it, lowestDis)
        }
        return lowestDis
    }

    val compare = (dat: Array[(Double, Data)]) => compareFunc(dat.iterator, (Double.PositiveInfinity, Sphere(null, 0)))

    val closestObject: (Double, Data) = compare(distances)

    return closestObject
}

def newObject(): Data = {
    print("Plane or Sphere? ")
    val input = readLine.toLowerCase
    val planeOrSphere = if (input.startsWith("p")) "plane" else if (input.startsWith("s")) "sphere" else "null"

    if (planeOrSphere != "sphere" && planeOrSphere != "plane") {
        throw new Exception("You have failed at choosing plane or sphere")
    }

    return if (planeOrSphere == "sphere") {
        val center: Vect3 = getThreeValues("center")

        val radius: Double = getOneValue("radius")

        Sphere(center, radius)

    } else {
        val normalVec: Vect3 = unitVector(getThreeValues("normal vector"))

        val d: Double = getOneValue("distance")

        Plane(normalVec, d)
    }
}

def main(): Int = {
    val r0: Vect3 = getThreeValues("start of ray")
    val rDir: Vect3 = unitVector(getThreeValues("dir of ray"))

    println(rDir)

    val numOfObjects: Int = getOneValue("Number of objects").toInt

    val newObjectFunc = (unused: Int) => newObject()

    val objects: Array[Data] = (0 until numOfObjects map newObjectFunc).toArray

    val x: (Double, Data) = intersection(r0, rDir, objects)
    println(s"Closest Object is at distance ${x._1}")

    return 0
}

main()


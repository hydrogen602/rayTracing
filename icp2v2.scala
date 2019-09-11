
import io.StdIn._

def getThreeValues(prompt: String): (Double, Double, Double) = {
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
        return (a, b, c)
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

def scale(ls: (Double, Double, Double), t: Double): (Double, Double, Double) = {

    return (ls._1 * t, ls._2 * t, ls._3 * t)
}

def addition(a: (Double, Double, Double), b: (Double, Double, Double)): (Double, Double, Double)  = {

    return (a._1 + b._1, a._2 + b._2, a._3 + b._3)
}

def dot(a: (Double, Double, Double), b: (Double, Double, Double)): Double = {

    return a._1 * b._1 + a._2 * b._2 + a._3 * b._3
}

def squareOfMag(a: (Double, Double, Double)): Double = {
    return dot(a, a)
}

def unitVector(v: (Double,Double,Double)): (Double, Double, Double) = {

    val mag = math.sqrt(squareOfMag(v))

    return (v._1 / mag, v._2 / mag, v._3 / mag)
}

def intersectionWithSphere(r0: (Double, Double, Double), r: (Double, Double, Double), n: (Double, Double, Double), radius: Double): Double = {
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
    println(thingInSquareRoot)

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

def intersectionWithPlane(r0: (Double, Double, Double), r: (Double, Double, Double), n: (Double, Double, Double), radius: Double): Double = {
    /* 
     * r0   =   start point of ray
     * r =   direction from start
     * n    =   normal List from origin to plane
     * 
     * returns scalar t that satisfies r(t) * n = d
     * where r(t) is r0 + r * t
     *
     * 
     */

     // see CS notebook for math
 
     return (d - dot(n, r0)) / dot(n, r)
}

def main(): Int = {
    val r0: (Double, Double, Double) = getThreeValues("start of ray")
    val rDir: (Double, Double, Double) = unitList(getThreeValues("dir of ray"))

    println(rDir)

    print("Plane or Sphere? ")
    val input = readLine.toLowerCase
    val planeOrSphere = if (input.startsWith("p")) "plane" else if (input.startsWith("s")) "sphere" else "null"


    if (planeOrSphere == "sphere") {
        val center: (Double, Double, Double) = getThreeValues("center")

        val radius: Double = getOneValue("radius")

        val t = intersectionWithSphere(r0, rDir, center, radius)

        println()

        println(s"distance = $t")

    } else if (planeOrSphere == "plane") {
        val normalList: (Double, Double, Double) = unitList(getThreeValues("normal vector"))

        val d: Double = getOneValue("distance")

        val t: Double = intersectionWithPlane(r0, rDir, normalList, d)

        println()

        println(s"distance = $t")

    } else if (planeOrSphere == "null") {
        println("You have failed at choosing plane or sphere")
        println("Exiting...")
        return -1
    }

    return 0
}

main()


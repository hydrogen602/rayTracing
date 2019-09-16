
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

class Ray(srcArg: Vect3, dirArg: Vect3, color: DColor) {
    val src: Vect3 = srcArg
    val dir: Vect3 = dirArg.normalize
    val c: DColor = color

    /*
    def trace(objects: Array[GeometricObject]): (Double, GeometricObject) = {

        def intersectionFunc(geometry: GeometricObject): Double = {
            geometry.intersection(this)
        }

        val solutionExists = (x: (Double, Data)) => x._1 >= 0 // funcs return -1 on failure, this weeds that out

        val distances: List[(Double, Data)] = sc.objects map intersectionFunc zip sc.objects filter solutionExists //min

        val minFunc = (a: (Double, Data), b: (Double, Data)) => if (a._1 < b._1) a else b

        return if (distances.length > 0) distances.reduce(minFunc) else (-1, null)
    }*/
}


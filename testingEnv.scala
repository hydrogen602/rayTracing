import io.StdIn._

class Vect3(xArg: Double, yArg: Double, zArg: Double) {

    val x: Double = xArg
    val y: Double = yArg
    val z: Double = zArg

    def apply(index: Int): Double = {
        // idk why i made this
        index match {
            case 0 => x
            case 1 => y
            case 2 => z
            case _ => throw new IndexOutOfBoundsException(s"$index")
        }
    }

    def +(other: Vect3): Vect3 = new Vect3(x + other.x, y + other.y, z + other.z)

    def -(other: Vect3): Vect3 = new Vect3(x - other.x, y - other.y, z - other.z)

    // scalar multiplication
    def *(sc: Double): Vect3 = new Vect3(x * sc, y * sc, z * sc)

    // scalar multiplication with 1/sc
    def /(sc: Double): Vect3 = new Vect3(x / sc, y / sc, z / sc)

    // dot product
    def *(other: Vect3): Double = x * other.x + y * other.y + z * other.z

    // cross product
    def x(v: Vect3): Vect3 = new Vect3(y * v.z - z * v.y, -(x * v.z - z * v.x), x * v.y - y * v.x)

    // |v|^2
    def squareOfMag(): Double = this * this

    // |v|
    def mag(): Double = math.sqrt(this * this)

    //  v / |v|
    def normalize(): Vect3 = this / mag() 

    // +v  <- idk why i implemented this
    def unary_+(): Vect3 = new Vect3(x, y, z)

    // -v
    def unary_-(): Vect3 = new Vect3(-x, -y, -z)

    // for debugging
    override def toString(): String = s"<$x, $y, $z>"
}

// Input functions

def getThreeValues(prompt: String): Vect3 = {
    print(s"${prompt}: ")
    val dataStr: Array[String] = readLine.split(" ")

    if (dataStr.length != 3) {
        println("Y are you so bad at life")
        println(s"${dataStr(0)} ${dataStr(1)} ${dataStr(2)}")
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
    print(s"${prompt}: ")
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

abstract class GeometricObject {
    def reflectRay(ray: Ray): Ray
    def intersection(r: Ray): Double
    def colorWithLight(): DColor
}

class Plane(normal: Vect3, dArg: Double, colorArg: DColor, refl: Double) extends GeometricObject {
    require(0 <= refl && refl <= 1)
    private val n: Vect3 = normal
    private val d: Double = dArg
    private val color: DColor = colorArg
    private val reflectivity: Double = refl // 0 is matte, 1 is a mirror

    def colorWithLight(): DColor = {
        val inLight = true // implement later
        
        return if (inLight) color else new DColor(0, 0, 0)
    }

    def reflectRay(ray: Ray): Ray = {
        if (reflectivity < 0.001) {
            // not reflective, no need to reflect
            return null
        }

        /*
         * r_reflected = r - 2 (r * n) n
         */

        val t: Double = intersection(ray)
        assert(t != -1, "reflectRay should only be called on intersecting rays")
        val pointReflected: Vect3 = ray.extend(t)

        val normal = n.normalize
        val dirReflected = ray.direction - (normal * 2 * (ray.direction * normal))

        /*
         * given a ray reflected of the surface of this objects,
         * this finds the new color of the reflected ray
         * 
         * Given reflectivity R in [0, 1]
         * the new color is (1-R) of the color of the object
         * and R of the color of the ray being reflected
         */
        assert(0 <= reflectivity && reflectivity <= 1, "reflectivity should be in [0, 1]")
        
        val colorReflected = (ray.color * reflectivity) + (color * (1 - reflectivity))

        return new Ray(pointReflected, dirReflected, colorReflected)
    }

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

         val tmp = (d - (n * r.source)) / (n * r.direction)
         return if (tmp == Double.PositiveInfinity || tmp == Double.NegativeInfinity) -1 else tmp
    }

    override def toString(): String = "Plane"
}

class Sphere(centerArg: Vect3, radiusArg: Double, colorArg: DColor, refl: Double) extends GeometricObject {
    require(0 <= refl && refl <= 1)
    private val center: Vect3 = centerArg
    private val radius: Double = radiusArg
    private val color: DColor = colorArg
    private val reflectivity: Double = refl // 0 is matte, 1 is a mirror

    def colorWithLight(): DColor = {
        val inLight = true // implement later
        
        return if (inLight) color else new DColor(0, 0, 0)
    }

    def reflectRay(ray: Ray): Ray = {
        if (reflectivity < 0.001) {
            // not reflective, no need to reflect
            return null
        }

        /*
         * r_reflected = r - 2 (r * n) n
         */

        val t: Double = intersection(ray)
        assert(t != -1, "reflectRay should only be called on intersecting rays")
        val pointReflected: Vect3 = ray.extend(t)

        val normal = (pointReflected - center).normalize
        val dirReflected = ray.direction - (normal * 2 * (ray.direction * normal))

        /*
         * given a ray reflected of the surface of this objects,
         * this finds the new color of the reflected ray
         * 
         * Given reflectivity R in [0, 1]
         * the new color is (1-R) of the color of the object
         * and R of the color of the ray being reflected
         */
        assert(0 <= reflectivity && reflectivity <= 1, "reflectivity should be in [0, 1]")
        
        val colorReflected = (ray.color * reflectivity) + (color * (1 - reflectivity))

        return new Ray(pointReflected, dirReflected, colorReflected)
    }

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
        val a = r.direction.squareOfMag
        val b = -2 * (center * r.direction) + 2 * (r.source * r.direction)
        val c = center.squareOfMag - 2 * (center * r.source) + r.source.squareOfMag - radius * radius

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


class Ray(srcArg: Vect3, dirArg: Vect3, colorArg: DColor) {
    private val src: Vect3 = srcArg
    private val dir: Vect3 = dirArg.normalize
    private val c: DColor = colorArg

    def source = src
    def direction = dir
    def color = c

    def trace(objects: Array[GeometricObject]): (Double, GeometricObject) = {

        def intersectionFunc(geometry: GeometricObject): Double = {
            geometry.intersection(this)
        }

        //val solutionExists = (x: (Double, GeometricObject)) => x._1 >= 0 // funcs return -1 on failure, this weeds that out

        val distances: Array[(Double, GeometricObject)] = objects map intersectionFunc zip objects filter (_._1 >= 0) // solutionExists

        val minFunc = (a: (Double, GeometricObject), b: (Double, GeometricObject)) => if (a._1 < b._1) a else b

        return if (distances.length > 0) distances.reduce(minFunc) else (-1, null)
    }

    def traceAndHitToDisplay(objects: Array[GeometricObject], counter: Int): (Double, DColor) = {
        /*
         * Find nearest point of intersection and return the color of the said object
         * If the object is reflective, reflect and find the next intersection
         * blend colors between intersections
         *
         * Note: returns (-1, null) upon failure aka not hitting something
         */
        val (d, obj) = trace(objects)
        if (d == -1) {
            return (-1, null)
        }
        
        val rReflected: Ray = obj.reflectRay(this)
        if (rReflected == null || counter > 0) {
            // no reflectivity  // stack overflow protection
            return (d, obj.colorWithLight)
        }

        return rReflected.traceAndHitToDisplay(objects, counter + 1)
    }

    def extend(t: Double): Vect3 = src + (dir * t)
}

class DColor(rArg: Double, gArg: Double, bArg: Double) {

    private val red = rArg
    private val green = gArg
    private val blue = bArg

    def assembleRGB(): Int = {
        require(0 <= red && red <= 255 && 0 <= green && green <= 255 && 0 <= blue && blue <= 255, 
            "rgb values not in range [0, 255]")
        red.toInt << 16 | green.toInt << 8 | blue.toInt
    }

    def +(other: DColor): DColor = new DColor(red + other.red, green + other.green, blue + other.blue)

    def -(other: DColor): DColor = new DColor(red - other.red, green - other.green, blue - other.blue)

    def *(sc: Double): DColor = new DColor(red * sc, green * sc, blue * sc)
}
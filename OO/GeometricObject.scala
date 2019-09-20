
abstract class GeometricObject {
    def reflectRay(ray: Ray): Ray
    def intersection(r: Ray): Double
    def colorWithLight(inLight: Double): DColor
    def getNormal(ray: Ray): Vect3
}

class Plane(normal: Vect3, dArg: Double, colorArg: DColor, refl: Double) extends GeometricObject {
    require(0 <= refl && refl <= 1)
    private val n: Vect3 = normal
    private val d: Double = dArg
    private val color: DColor = colorArg
    private val reflectivity: Double = refl // 0 is matte, 1 is a mirror

    def colorWithLight(inLight: Double): DColor = {
        require(0 <= inLight && inLight <= 1, "inLight has to be in [0, 1]")
        return color * inLight
    }

    def getNormal(ray: Ray): Vect3 = {
        return n.normalize
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
        
        val colorReflected: DColor = (ray.color * reflectivity) + (color * (1 - reflectivity))

        return new Ray(pointReflected + (dirReflected * 0.0001), dirReflected, colorReflected)
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

    def colorWithLight(inLight: Double): DColor = {
        require(0 <= inLight && inLight <= 1, "inLight has to be in [0, 1]")
        return color * inLight
    }

    def getNormal(ray: Ray): Vect3 = {
        val t: Double = intersection(ray)
        assert(t != -1, "getNormal should only be called on intersecting rays")
        val pointReflected: Vect3 = ray.extend(t)

        return (pointReflected - center).normalize
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

        return new Ray(pointReflected + (dirReflected * 0.0001), dirReflected, colorReflected)
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

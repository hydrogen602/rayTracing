


class Sphere(private val center: Vect3, private val radius: Double, val color: DColor, val reflectivity: Double) extends GeometricObject {
    require(0 <= reflectivity && reflectivity <= 1)
    // 0 is matte, 1 is a mirror

    def getNormal(ray: Ray): Vect3 = {
        val t: Double = intersection(ray)
        assert(t != -1, "getNormal should only be called on intersecting rays")
        val pointReflected: Vect3 = ray.extend(t)

        return (pointReflected - center).normalize
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

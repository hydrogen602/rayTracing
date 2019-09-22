
class Ray(srcArg: Vect3, dirArg: Vect3, colorArg: DColor) {
    private val src: Vect3 = srcArg
    private val dir: Vect3 = dirArg.normalize
    private val c: DColor = colorArg

    def source = src
    def direction = dir
    def color = c

    private def trace(objects: Array[GeometricObject]): (Double, GeometricObject) = {

        def intersectionFunc(geometry: GeometricObject): Double = {
            geometry.intersection(this)
        }

        //val solutionExists = (x: (Double, GeometricObject)) => x._1 >= 0 // funcs return -1 on failure, this weeds that out

        val distances: Array[(Double, GeometricObject)] = objects map intersectionFunc zip objects filter (_._1 > 0.000001) 
        // solutionExists at some distance (not colliding with itself)

        val minFunc = (a: (Double, GeometricObject), b: (Double, GeometricObject)) => if (a._1 < b._1) a else b

        return if (distances.length > 0) distances.reduce(minFunc) else (-1, null)
    }

    def traceAndHitToDisplay(objects: Array[GeometricObject], lsrc: LightSource, counter: Int = 0): (Double, DColor) = {
        /*
         * Find nearest point of intersection and return the color of the said object
         * If the object is reflective, reflect and find the next intersection
         * blend colors between intersections
         *
         * Note: returns (-1, null) upon failure aka not hitting something
         */
        val (d, obj) = trace(objects)
        if (d == -1) {
            return (Double.PositiveInfinity, DColor(0, 0, 0)) // nothing hit
        }

        if (obj.reflectivity < 0.0001 || counter > 3) {
            // no reflectivity or overflow protection
            // just return the objects color + shading

            val pointOfHit: Vect3 = extend(d)
            val length = (lsrc.point - pointOfHit).mag
            val dir = lsrc.point - pointOfHit
            val lightRay = new Ray(pointOfHit + (dir.normalize * 0.001), dir, null)
            val (dShadow, objShadow) = lightRay.trace(objects)

            return if (dShadow >= length || dShadow == -1) {
                // dNew >= length means theres nothing between the light and the point
                // dNew == -1 means theres nothing at all on this ray
                // also nothing blocking the light
                //println(s"$dNew, $objNew $pointOfHit, ${lsrc.point - pointOfHit}")

                val shading = dir.normalize * obj.getNormal(this)
                
                (d, obj.color * shading)
            }
            else {
                (d, DColor(0, 0, 0))
            }
        }
        
        val rReflected: Ray = obj.reflectRay(this)

        val newResult = rReflected.traceAndHitToDisplay(objects, lsrc, counter + 1)

        return if (newResult._1 == -1) (d, rReflected.color) else newResult
    }

    def extend(t: Double): Vect3 = src + (dir * t)
}
package rayTracing

import data._
import geometricObject._

class Ray private(private val src: Vect3, private val dir: Vect3) {

    def source = src
    def direction = dir

    private def colorShading(objects: Array[GeometricObject], lsrc: LightSource, d: Double, obj: GeometricObject): DColor = {
        val pointOfHit: Vect3 = extend(d)

        val lightColor = lsrc.color

        val length = (lsrc.point - pointOfHit).mag
        val dir = lsrc.point - pointOfHit
        val lightRay = Ray(pointOfHit + (dir.normalize * 0.00001), dir)
        val (dShadow, objShadow) = lightRay.trace(objects)

        return if (dShadow >= length || dShadow == -1) {
            // dNew >= length means theres nothing between the light and the point
            // dNew == -1 means theres nothing at all on this ray
            // also nothing blocking the light
            //println(s"$dNew, $objNew $pointOfHit, ${lsrc.point - pointOfHit}")

            val shading = dir.normalize * obj.getNormal(this)

            require(-1 <= shading && shading <= 1, s"shading should be in [0, 1], instead $shading")

            lsrc.applyColor(obj.color * math.abs(shading))         
        }
        else {
            DColor(0, 0, 0) // in the shadow
        }
    }

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

        if (counter > 3) {
            // overflow protection
            // just return the objects color + shading

            val color = colorShading(objects, lsrc, d, obj)
            return (d, color)
        }
        
        val rReflected: Ray = obj.reflectRay(this) match {
            case Some(r) => r
            case None => return (d, colorShading(objects, lsrc, d, obj))
        }

        val (dNext, colorNext) = rReflected.traceAndHitToDisplay(objects, lsrc, counter + 1)

        /*
         * given a ray reflected of the surface of this objects,
         * this finds the new color of the reflected ray
         * 
         * Given reflectivity R in [0, 1]
         * the new color is (1-R) of the color of the object
         * and R of the color of the ray being reflected
         */
        
        val shadedColor = colorShading(objects, lsrc, d, obj)

        val colorReflected = (colorNext * obj.reflectivity) + (shadedColor * (1 - obj.reflectivity))

        return if (dNext == Double.PositiveInfinity) (dNext, shadedColor) else (dNext, colorReflected)
    }

    def extend(t: Double): Vect3 = src + (dir * t)
}

object Ray {
    def apply(src: Vect3, dir: Vect3): Ray = {
        new Ray(src, dir.normalize)
    }
}
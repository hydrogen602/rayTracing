package rayTracing.geometricObject

import rayTracing._
import rayTracing.data._

trait GeometricObject {
    val color: DColor
    val reflectivity: Double

    def reflectRay(ray: Ray): Ray = {
        //if (reflectivity < 0.001) {
        //    // not reflective, no need to reflect
        //    return null
        //}

        /*
         * r_reflected = r - 2 (r * n) n
         */

        val t: Double = intersection(ray)
        assert(t != -1, "reflectRay should only be called on intersecting rays")
        val pointReflected: Vect3 = ray.extend(t)

        val normal = getNormal(ray)
        val dirReflected = ray.direction - (normal * 2 * (ray.direction * normal))

        return new Ray(pointReflected + (dirReflected * 0.00001), dirReflected)
    }

    def intersection(r: Ray): Double

    def getNormal(ray: Ray): Vect3
}
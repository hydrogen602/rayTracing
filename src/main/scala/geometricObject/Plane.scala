package rayTracing.geometricObject

import rayTracing._
import rayTracing.data._

class Plane(private val n: Vect3, private val d: Double, val color: DColor, val reflectivity: Double) extends GeometricObject {
    require(0 <= reflectivity && reflectivity <= 1)
    // 0 is matte, 1 is a mirror

    def getNormal(ray: Ray): Vect3 = {
        return n.normalize
    }

    def intersection(r: Ray): Option[Double] = {
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
        if (tmp == Double.PositiveInfinity || tmp == Double.NegativeInfinity || tmp < 0) None else Some(tmp)
    }

    override def toString(): String = "Plane"
}
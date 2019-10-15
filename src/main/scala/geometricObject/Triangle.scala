package rayTracing.geometricObject

import rayTracing._
import rayTracing.data._

class Triangle(private val a: Vect3, private val b: Vect3, private val c: Vect3, val color: DColor, val reflectivity: Double) extends GeometricObject {
    require(0 <= reflectivity && reflectivity <= 1)
    // 0 is matte, 1 is a mirror

    private val n: Vect3 = ((c - a) x (c - b)).normalize // negative?

    private val d: Double = n * a //if (n * a > 0) n * a else -(n * a) // distance to origin
    // idk if that math works

    private val ab: Vect3 = b - a
    private val ac: Vect3 = c - a
    private val bc: Vect3 = c - b

    println("triangle")
    println(n)
    println(d)

    def getNormal(ray: Ray): Vect3 = {
        return n // n is already normalized
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
        val planeIntersectDis = if (tmp == Double.PositiveInfinity || tmp == Double.NegativeInfinity || tmp < 0) None else Some(tmp)
        
        planeIntersectDis match {
            case None => None
            case Some(distance) => {
                val p = r.extend(distance)
                val line = Line(p, ab)
                
                ???
            }
        }
    }

    override def toString(): String = "Triangle"
}
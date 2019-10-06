package rayTracing

import data._
import geometricObject._

case class RayHit(dis: Double, obj: GeometricObject)

class Ray private(private val src: Vect3, private val dir: Vect3) {

    def source = src
    def direction = dir

    private def colorShading(objects: Array[GeometricObject], lsrc: LightSource, d: Double, obj: GeometricObject): DColor = {
        val pointOfHit: Vect3 = extend(d)

        val lightColor = lsrc.color

        val length = (lsrc.point - pointOfHit).mag
        val dir = lsrc.point - pointOfHit
        val lightRay = Ray(pointOfHit + (dir.normalize * 0.00001), dir)
        lightRay.trace(objects) match {
            case None => {
                // light ray hit nothing
                val shading = dir.normalize * obj.getNormal(this)

                require(-1 <= shading && shading <= 1, s"shading should be in [0, 1], instead $shading")

                lsrc.applyColor(obj.color * math.abs(shading))  
            }
            case Some(h) => {
                if (h.dis >= length) {
                    // light ray hit something after the light
                    val shading = dir.normalize * obj.getNormal(this)

                    require(-1 <= shading && shading <= 1, s"shading should be in [0, 1], instead $shading")

                    lsrc.applyColor(obj.color * math.abs(shading)) 
                }
                else {
                    DColor(0, 0, 0)
                }
            }
        }
    }

    private def trace(objects: Array[GeometricObject]): Option[RayHit] = {

        def intersectionFunc(geometry: GeometricObject): Option[RayHit] = {
            geometry.intersection(this) match {
                // dont hit itself
                case Some(d) => if (d < 0.000001) None else Some(RayHit(d, geometry))
                case None => None
            }
        }

        val distances: Array[RayHit] = SceneGeometry.objects.map(intersectionFunc).filter(x => x != None).map(_.get)
        // solutionExists at some distance (not colliding with itself)

        if (distances.length > 0) Some(distances.reduce((a, b) => if (a.dis < b.dis) a else b)) else None
    }

    def traceAndHitToDisplay(lsrc: LightSource, counter: Int = 0): (Double, DColor) = {
        /*
         * Find nearest point of intersection and return the color of the said object
         * If the object is reflective, reflect and find the next intersection
         * blend colors between intersections
         *
         * Note: returns (-1, null) upon failure aka not hitting something
         */
        val hit: RayHit = trace(SceneGeometry.objects) match {
            case Some(value) => value
            case None => {
                return (Double.PositiveInfinity, DColor(0, 0, 0)) // nothing hit
            }
        }

        if (counter > 3) {
            // overflow protection
            // just return the objects color + shading

            val color = colorShading(SceneGeometry.objects, lsrc, hit.dis, hit.obj)
            return (hit.dis, color)
        }
        
        val rReflected: Ray = hit.obj.reflectRay(this) match {
            case Some(r) => r
            case None => {return (hit.dis, colorShading(SceneGeometry.objects, lsrc, hit.dis, hit.obj)); ???}
        }

        val (dNext, colorNext) = rReflected.traceAndHitToDisplay(lsrc, counter + 1)

        /*
         * given a ray reflected of the surface of this objects,
         * this finds the new color of the reflected ray
         * 
         * Given reflectivity R in [0, 1]
         * the new color is (1-R) of the color of the object
         * and R of the color of the ray being reflected
         */
        
        val shadedColor = colorShading(SceneGeometry.objects, lsrc, hit.dis, hit.obj)

        val colorReflected = (colorNext * hit.obj.reflectivity) + (shadedColor * (1 - hit.obj.reflectivity))

        return if (dNext == Double.PositiveInfinity) (dNext, shadedColor) else (dNext, colorReflected)
    }

    def extend(t: Double): Vect3 = src + (dir * t)
}

object Ray {
    def apply(src: Vect3, dir: Vect3): Ray = {
        new Ray(src, dir.normalize)
    }
}
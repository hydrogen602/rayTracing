package rayTracing

import data._
import geometricObject._

case class RayHit(dis: Double, obj: GeometricObject)

class Ray private(private val src: Vect3, private val dir: Vect3) {

    def source = src
    def direction = dir

    private def colorShading(lsrc: LightSource, hit: RayHit): DColor = {
        val pointOfHit: Vect3 = extend(hit.dis)

        val length = (lsrc.point - pointOfHit).mag
        val dir = lsrc.point - pointOfHit
        val lightRay = Ray(pointOfHit + (dir.normalize * 0.00001), dir)
        lightRay.trace() match {
            case None => {
                // light ray hit nothing
                val shading = dir.normalize * hit.obj.getNormal(this)

                require(-1 <= shading && shading <= 1, s"shading should be in [0, 1], instead $shading")

                lsrc.applyColor(hit.obj.color * math.abs(shading))  
            }
            case Some(h) => {
                if (h.dis >= length) {
                    // light ray hit something after the light
                    val shading = dir.normalize * hit.obj.getNormal(this)

                    require(-1 <= shading && shading <= 1, s"shading should be in [0, 1], instead $shading")

                    lsrc.applyColor(hit.obj.color * math.abs(shading)) 
                }
                else {
                    DColor(0, 0, 0)
                }
            }
        }
    }

    private def trace(): Option[RayHit] = {
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

    def traceAndHitToDisplay(lsrc: LightSource, counter: Int = 0): Option[DColor] = {
        /*
         * Find nearest point of intersection and return the color of the said object
         * If the object is reflective, reflect and find the next intersection
         * blend colors between intersections
         *
         * returns None if nothing was hit
         * returns Some(DColor) for the hit object
         */
        trace() match {
            case None => {
                return None // nothing hit
            }
            case Some(hit) => {
                if (counter > 3) {
                    // overflow protection
                    // just return the objects color + shading
        
                    Some(colorShading(lsrc, hit))
                }
                else {
                    hit.obj.reflectRay(this) match {
                        case None => Some(colorShading(lsrc, hit))
                        case Some(rReflected) => {
                            val shadedColor = colorShading(lsrc, hit)
                            /*
                            * given a ray reflected of the surface of this objects,
                            * this finds the new color of the reflected ray
                            * 
                            * Given reflectivity R in [0, 1]
                            * the new color is (1-R) of the color of the object
                            * and R of the color of the ray being reflected
                            */
                            rReflected.traceAndHitToDisplay(lsrc, counter + 1) match {
                                case None => Some(shadedColor)
                                case Some(colorNext) => Some((colorNext * hit.obj.reflectivity) + (shadedColor * (1 - hit.obj.reflectivity)))
                            }
                        }
                    }
                }
            }
        }
    }

    def extend(t: Double): Vect3 = src + (dir * t)
}

object Ray {
    def apply(src: Vect3, dir: Vect3): Ray = {
        new Ray(src, dir.normalize)
    }
}
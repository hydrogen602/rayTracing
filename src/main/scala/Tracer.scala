package rayTracing

import rayTracing.data._
import rayTracing.geometricObject._

object Tracer {
    // def colorShading(r: Ray, lsrc: LightSource, hit: RayHit): DColor = ???

    // def colorMixing(colorThis: DColor, colorNext: DColor, reflectivity: Double): DColor = {
    //     (colorNext * reflectivity) + (colorThis * (1 - reflectivity))
    // }

    // def traceAndHitToDisplay(r: Ray, lsrc: LightSource, counter: Int = 0): (Double, DColor) = {
    //     /*
    //      * Find nearest point of intersection and return the color of the said object
    //      * If the object is reflective, reflect and find the next intersection
    //      * blend colors between intersections
    //      *
    //      * Note: returns (-1, null) upon failure aka not hitting something
    //      */
    //     trace(r) match {
    //         case None => (Double.PositiveInfinity, DColor(0, 0, 0)) // nothing hit
    //         case Some(hit) => {
    //             if (counter > 3) {
    //                 // overflow protection
    //                 // just return the objects color + shading
        
    //                 (hit.dis, colorShading(r, lsrc, hit))
    //             }
    //             else {
    //                 val rReflected: Ray = hit.obj.reflectRay(r) match {
    //                     case None => return (hit.dis, colorShading(r, lsrc, hit))
    //                     case Some(r) => r
    //                 }
            
    //                 val (dNext, colorNext) = traceAndHitToDisplay(rReflected, lsrc, counter + 1)
            
    //                 /*
    //                 * given a ray reflected of the surface of this objects,
    //                 * this finds the new color of the reflected ray
    //                 * 
    //                 * Given reflectivity R in [0, 1]
    //                 * the new color is (1-R) of the color of the object
    //                 * and R of the color of the ray being reflected
    //                 */
                    
    //                 val shadedColor = colorShading(r,lsrc, hit)
    //                 if (dNext == Double.PositiveInfinity) {
    //                     (dNext, shadedColor)
    //                 }
    //                 else {
    //                     (dNext, colorMixing(shadedColor, colorNext, hit.obj.reflectivity))
    //                 }
    //             }
    //         }
    //     }
    // }
}
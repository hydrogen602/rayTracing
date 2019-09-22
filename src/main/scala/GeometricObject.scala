
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

        /*
         * given a ray reflected of the surface of this objects,
         * this finds the new color of the reflected ray
         * 
         * Given reflectivity R in [0, 1]
         * the new color is (1-R) of the color of the object
         * and R of the color of the ray being reflected
         */
        
        val colorReflected = (ray.color * reflectivity) + (color * (1 - reflectivity))

        return new Ray(pointReflected + (dirReflected * 0.0001), dirReflected, colorReflected)
    }

    def intersection(r: Ray): Double

    def getNormal(ray: Ray): Vect3
}

abstract class GeometricObject {
    def reflectRay(ray: Ray): Ray
    def intersection(r: Ray): Double
    def colorWithLight(inLight: Double): DColor
    def getNormal(ray: Ray): Vect3
}
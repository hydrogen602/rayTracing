package rayTracing

import rayTracing.data._
import rayTracing.geometricObject._

object SceneGeometry {
    val objects: Array[GeometricObject] = loadObjects("objects.xml")

    def stringToVect(s: String): Vect3 = {
        val Array(a, b, c) = s.split(",")
        Vect3(a.toDouble, b.toDouble, c.toDouble)
      }
    
      def stringToDColor(s: String): DColor = {
        val Array(a, b, c) = s.split(",")
        DColor(a.toDouble, b.toDouble, c.toDouble)
    }

    def loadObjects(xmlFileName: String): Array[GeometricObject] = {
        def generateSphere(n: xml.Node): Sphere = {
          val center: Vect3 = stringToVect((n \ "center").text)
          val radius: Double = (n \ "radius").text.toDouble
          val color: DColor = stringToDColor((n \ "color").text)
          val refl: Double = (n \ "reflectivity").text.toDouble
          require(0 <= refl && refl <= 1, "Reflectivity must be in range [0, 1]")
    
          new Sphere(center, radius, color, refl)
        }
    
        def generatePlane(n: xml.Node): Plane = {
          val normal: Vect3 = stringToVect((n \ "normal").text)
          val d: Double = (n \ "distance").text.toDouble
          val color: DColor = stringToDColor((n \ "color").text)
          val refl: Double = (n \ "reflectivity").text.toDouble
          require(0 <= refl && refl <= 1, "Reflectivity must be in range [0, 1]")
    
          new Plane(normal, d, color, refl)
        }

        def generateTriangle(n: xml.Node): Triangle = {
          val a: Vect3 = stringToVect((n \ "a").text)
          val b: Vect3 = stringToVect((n \ "b").text)
          val c: Vect3 = stringToVect((n \ "c").text)
          val color: DColor = stringToDColor((n \ "color").text)
          val refl: Double = (n \ "reflectivity").text.toDouble
          require(0 <= refl && refl <= 1, "Reflectivity must be in range [0, 1]")
    
          new Triangle(a, b, c, color, refl)
        }
    
        val xmlObjects = xml.XML.loadFile(xmlFileName)
        val spheres = (xmlObjects \ "sphere").map(generateSphere).toArray
        val planes = (xmlObjects \ "plane").map(generatePlane).toArray
        val triangles = (xmlObjects \ "triangle").map(generateTriangle).toArray
        spheres ++ planes ++ triangles
    }
}
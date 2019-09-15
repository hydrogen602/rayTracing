import io.StdIn._

// Vectors
case class Vect3(x: Double, y: Double, z: Double)

// Geometric Objects
abstract class Data
case class Plane(vect: Vect3, d: Double) extends Data
case class Sphere(vect: Vect3, d: Double) extends Data

// Collection of Objects
case class SceneObjects(objects: List[Data])

// Light ray with color
case class Ray(src: Vect3, dir: Vect3, Color: (Int, Int, Int))


// Input functions

def getThreeValues(prompt: String): Vect3 = {
    print(s"${prompt}: ")
    val dataStr: Array[String] = readLine.split(" ")

    if (dataStr.length != 3) {
        println("Y are you so bad at life")
        println(s"${dataStr(0)} ${dataStr(1)} ${dataStr(2)}")
        return getThreeValues(prompt)
    }

    try {
        val nums: Array[Double] = dataStr.map(_.trim.toDouble)
        return Vect3(nums(0), nums(1), nums(2))
    }
    catch {
        case x: NumberFormatException => {
            println("Y are you so bad at life")
            println(dataStr(0) +" "+ dataStr(1) +" "+ dataStr(2))
        }
    }
    return getThreeValues(prompt)
}

def getOneValue(prompt: String): Double = {
    print(s"${prompt}: ")
    val ln = readLine

    try {
        return ln.trim.toDouble
    }
    catch {
        case x: NumberFormatException => {
            println("Y are you so bad at life")
            println(ln)
        }
    }
    return getOneValue(prompt)
}
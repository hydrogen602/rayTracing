 
class Plane(xArg: Double, yArg: Double, zArg: Double, dArg: Double) {
    val x: Double = xArg
    val y: Double = yArg
    val z: Double = zArg
    val d: Double = dArg

    override def toString(): String = {
        return s"Plane: n=<$x, $y, $z> d=$d"
    }
}

class Sphere(xArg: Double, yArg: Double, zArg: Double, rArg: Double) {
    val x: Double = xArg
    val y: Double = yArg
    val z: Double = zArg
    val r: Double = rArg

    override def toString(): String = {
        return s"Sphere: center=($x, $y, $z) r=$r"
    }
}

var p = new Plane(1,2,3, 10)
var s = new Sphere(1,1,1, 10)

println("s = " + s)
println("x = " + p.x)
println(p)

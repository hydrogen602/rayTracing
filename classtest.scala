
class Plane(xArg: Double, yArg: Double, zArg: Double, dArg: Double) {
    var x: Double = xArg
    var y: Double = yArg
    var z: Double = zArg
    var d: Double = dArg

    override def toString(): String = {
        return "Plane"
    }
}

var p = new Plane(1,2,3, 10)

println("x = " + p.x)
println(p)
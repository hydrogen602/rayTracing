
class Vect3(xArg: Double, yArg: Double, zArg: Double) {

    val x: Double = xArg
    val y: Double = yArg
    val z: Double = zArg

    def apply(index: Int): Double = {
        index match {
            case 0 => x
            case 1 => y
            case 2 => z
            case _ => throw new IndexOutOfBoundsException(s"$index")
        }
    }

    def +(other: Vect3): Vect3 = new Vect3(x + other.x, y + other.y, z + other.z)

    def -(other: Vect3): Vect3 = new Vect3(x - other.x, y - other.y, z - other.z)

    def *(sc: Double): Vect3 = new Vect3(x * sc, y * sc, z * sc)

    def /(sc: Double): Vect3 = new Vect3(x / sc, y / sc, z / sc)

    // dot product
    def *(other: Vect3): Double = x * other.x + y * other.y + z * other.z

    // cross product
    def x(v: Vect3): Vect3 = new Vect3(y * v.z - z * v.y, -(x * v.z - z * v.x), x * v.y - y * v.x)

    def squareOfMag(): Double = this * this

    def mag(): Double = math.sqrt(this * this)

    def normalize(): Vect3 = this / mag() 

    def unary_+(): Vect3 = new Vect3(x, y, z)

    def unary_-(): Vect3 = new Vect3(-x, -y, -z)

    override def toString(): String = s"<$x, $y, $z>"
}


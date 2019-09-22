
class Vect3 private(val x: Double, val y: Double, val z: Double) {

    def apply(index: Int): Double = {
        // idk why i made this
        index match {
            case 0 => x
            case 1 => y
            case 2 => z
            case _ => throw new IndexOutOfBoundsException(s"$index")
        }
    }

    def +(other: Vect3): Vect3 = Vect3(x + other.x, y + other.y, z + other.z)

    def -(other: Vect3): Vect3 = Vect3(x - other.x, y - other.y, z - other.z)

    // scalar multiplication
    def *(sc: Double): Vect3 = Vect3(x * sc, y * sc, z * sc)

    // scalar multiplication with 1/sc
    def /(sc: Double): Vect3 = Vect3(x / sc, y / sc, z / sc)

    // dot product
    def *(other: Vect3): Double = x * other.x + y * other.y + z * other.z

    // cross product
    def x(v: Vect3): Vect3 = Vect3(y * v.z - z * v.y, -(x * v.z - z * v.x), x * v.y - y * v.x)

    // |v|^2
    def squareOfMag(): Double = this * this

    // |v|
    def mag(): Double = math.sqrt(this * this)

    //  v / |v|
    def normalize(): Vect3 = this / mag() 

    // +v  <- idk why i implemented this
    def unary_+(): Vect3 = Vect3(x, y, z)

    // -v
    def unary_-(): Vect3 = Vect3(-x, -y, -z)

    // for debugging
    override def toString(): String = s"<$x, $y, $z>"
}

object Vect3 {
    def apply(x: Double, y: Double, z: Double): Vect3 = {
        new Vect3(x, y, z)
    }
}
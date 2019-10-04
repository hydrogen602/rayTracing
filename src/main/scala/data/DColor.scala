package rayTracing.data

import rayTracing._

class DColor private(val red: Double, val green: Double, val blue: Double) extends Iterable[Double] {

    def assembleRGB(): Int = {
        require(0 <= red && red <= 255 && 0 <= green && green <= 255 && 0 <= blue && blue <= 255, 
            s"rgb values not in range [0, 255], instead ($red, $green, $blue)")
        red.toInt << 16 | green.toInt << 8 | blue.toInt
    }

    def +(other: DColor): DColor = new DColor(red + other.red, green + other.green, blue + other.blue)

    def -(other: DColor): DColor = new DColor(red - other.red, green - other.green, blue - other.blue)

    def *(sc: Double): DColor = new DColor(red * sc, green * sc, blue * sc)

    def ==(d: DColor): Boolean = {
        math.abs(red - d.red) < 0.001 && math.abs(green - d.green) < 0.001 && math.abs(blue - d.blue) < 0.001
    }

    def !=(d: DColor): Boolean = {
        !(this == d)
    }

    def iterator: Iterator[Double] = Iterator(red, green, blue)

    // for debugging
    override def toString(): String = s"($red, $green, $blue)"
}

object DColor {

    def apply(r: Double, g: Double, b: Double): DColor = {
        new DColor(r, g, b)
    }
    def apply(v: Vect3): DColor = {
        new DColor(v.x, v.y, v.z)
    }
    def apply(i: Iterator[Double]): DColor = {
        val red = i.next
        val green = i.next
        val blue = i.next
        new DColor(red, green, blue)
    }
}

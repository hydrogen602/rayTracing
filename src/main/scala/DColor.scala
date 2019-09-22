
class DColor private(private val red: Double, private val green: Double, private val blue: Double) {

    def assembleRGB(): Int = {
        require(0 <= red && red <= 255 && 0 <= green && green <= 255 && 0 <= blue && blue <= 255, 
            s"rgb values not in range [0, 255], instead ($red, $green, $blue)")
        red.toInt << 16 | green.toInt << 8 | blue.toInt
    }

    def +(other: DColor): DColor = new DColor(red + other.red, green + other.green, blue + other.blue)

    def -(other: DColor): DColor = new DColor(red - other.red, green - other.green, blue - other.blue)

    def *(sc: Double): DColor = new DColor(red * sc, green * sc, blue * sc)
}

object DColor {
    def apply(r: Double, g: Double, b: Double): DColor = {
        new DColor(r, g, b)
    }
    def apply(v: Vect3): DColor = {
        new DColor(v.x, v.y, v.z)
    }
}

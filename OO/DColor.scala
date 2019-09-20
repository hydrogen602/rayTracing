
class DColor(rArg: Double, gArg: Double, bArg: Double) {

    private val red = rArg
    private val green = gArg
    private val blue = bArg

    def assembleRGB(): Int = {
        require(0 <= red && red <= 255 && 0 <= green && green <= 255 && 0 <= blue && blue <= 255, 
            "rgb values not in range [0, 255]")
        red.toInt << 16 | green.toInt << 8 | blue.toInt
    }

    def +(other: DColor): DColor = new DColor(red + other.red, green + other.green, blue + other.blue)

    def -(other: DColor): DColor = new DColor(red - other.red, green - other.green, blue - other.blue)

    def *(sc: Double): DColor = new DColor(red * sc, green * sc, blue * sc)
}
package rayTracing.data

class LightSource private(val point: Vect3, val color: DColor) {

    def applyColor(d: DColor): DColor = {
        val r = if (color.red < d.red) color.red else d.red
        val g = if (color.green < d.green) color.green else d.green
        val b = if (color.blue < d.blue) color.blue else d.blue
        DColor(r, g, b)
    }
}

object LightSource {
    def apply(point: Vect3, color: DColor): LightSource = {
        new LightSource(point, color)
    }
}

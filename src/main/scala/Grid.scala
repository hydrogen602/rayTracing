

class Grid(raySource: Vect3, forwardArg: Vect3, upArg: Vect3, sideArg: Int) {
    require(forwardArg.squareOfMag == 1 && upArg.squareOfMag == 1)

    val src: Vect3 = raySource
    val forward: Vect3 = forwardArg
    val up: Vect3 = upArg
    val left: Vect3 = up x forward

    val side: Int = sideArg

    val centerOfPixelGrid = raySource + forward

    val topLeftCorner = centerOfPixelGrid + up + left
    val bottomRightCorner = centerOfPixelGrid - up - left

    val scalar = side - 1
    /*
     * Think of a grid. It has squares, but for simplicity I'm going to shoot the rays
     * at the corners of the box. Now there is one less square lengthwise than there
     * are corners between the squares, and so that is why I'm subtracting one here.
     * 
     *   X--X--X   Note: There are 3 corners (X) across, but 2 line segments (--)
     *   |  |  |     So I need to divide the side of the square grid by 2
     *   X--X--X     instead of 3 to get the distance between corners.
     *   |  |  | 
     *   X--X--X 
     */

    val rightStepVector = (-left * 2) / scalar
    val downStepVector = (-up * 2) / scalar

    def getPoint(i: Int, j: Int): Vect3 = {
        // returns the x, y, z of point (i, j) in the grid
        topLeftCorner + (rightStepVector * i) + (downStepVector * j)
    }

    def rayTraceOnce(objects: Array[GeometricObject], lsrc: LightSource, i: Int, j: Int): DColor = {
        require(0 <= i && i <= side && 0 <= j && j <= side, "rayTraceOnce: Index out of bounds")

        val point = getPoint(i, j)
        val ray = new Ray(src, point - src, DColor(0, 0, 0))

        val (t, color) = ray.traceAndHitToDisplay(objects, lsrc, 0)

        // t of -1 represents nothing was hit
        return if (t == -1) DColor(0, 0, 255) else color
    }
}
package rayTracing.data

case class Line(start: Vect3, dir: Vect3) {

    def compareDouble(a: Double, b: Double): Boolean = {
        math.abs(b - a) < 0.0001
    }

    def point(t: Double): Vect3 = {
        start + dir * t
    }

    def intersection(l: Line): Option[Vect3] = {
        val dim = 3
        // a = this; b = l
        //
        // ax*t + ax0 = bx*s + bx0
        // ay*t + ay0 = by*s + by0
        // az*t + az0 = bz*s + bz0
        // 
        // for var v
        // av*t - bv*s = bv0 - av0
        // [ av, -bv, bv0-av0 ]
        //

        var matrix = Array(
            Vect3(dir.x, -l.dir.x, start.x - l.start.x),
            Vect3(dir.y, -l.dir.y, start.y - l.start.y),
            Vect3(dir.z, -l.dir.z, start.z - l.start.z)
            )

        /*
         * Matrix
         * [ a  b : c ]
         * [ d  e : f ]
         * [ g  h : i ]
         * 
         * REF
         * [ 1  b/a :          c/a            ]
         * [ 0   1  : (f-c(d/a)) / (e-b(d/a)) ]
         * [ 0   1  : (i-c(g/a)) / (h-b(d/a)) ]
         * 
         * RREF
         * [ 1   0  : c/a - (b/a)(f-c(d/a)) / (e-b(d/a)) ]
         * [ 0   1  : (f-c(d/a)) / (e-b(d/a)) ]
         * [ 0   1  : (i-c(g/a)) / (h-b(d/a)) ]
         * 
         * t = c/a - (b/a)(f-c(d/a)) / (e-b(d/a))
         * s = (f-c(d/a)) / (e-b(d/a)) = (i-c(g/a)) / (h-b(g/a))
         * 
         */

        //matrix.foreach(println)

        def findNonZeroRow(matrix: Array[Vect3], index: Int, i: Int = 0): Option[Int] = {
            if (matrix(i)(0) != 0) {
                Some(i)
            }
            else if (i + 1 < dim) {
                findNonZeroRow(matrix, index, i+1)
            }
            else {
                None
            }
        }

        matrix.foreach(println)

        findNonZeroRow(matrix, 0) match {
            case None => {}
            case Some(index) => {
                val tmp = matrix(index)
                matrix(index) = matrix(0)
                matrix(0) = tmp / tmp(0)
            }
        }

        for (colIndex <- 0 until dim - 1) {
            for {
                i <- (colIndex) until dim 
                if (matrix(i)(colIndex) != 0)
            } {
                matrix(i) = matrix(i) / matrix(i)(colIndex)
            }

            matrix.foreach(println)
            println()

            for (i <- (colIndex + 1) until dim) {
                matrix(i) = matrix(i) - matrix(colIndex)
            }

            matrix.foreach(println)
            println()
        }
        // REF achieved

        var s = 0.0
        var t = 0.0

        val lastRow = matrix(2)
        if (compareDouble(0, lastRow(0)) && compareDouble(0, lastRow(1))) {
            // last one complete canceled out
            // format: 0*t + 0*s = c
            if (!compareDouble(0, lastRow(2))) {
                // 0 = c where c != 0
                // impossible state
                return None
            }
        }
        else { throw new IllegalStateException(s"(1) the equation solver sucks, got ${matrix.mkString("\n")}") } 

        val midRow = matrix(1)
        if (compareDouble(0, midRow(0)) && compareDouble(1, midRow(1))) {
            // format: 0*t + s = c

            s = midRow(2)
        }
        else { throw new IllegalStateException(s"(2) the equation solver sucks, got ${matrix.mkString("\n")}") } 

        matrix(0) = matrix(0) - matrix(1) * matrix(0)(1)

        val firstRow = matrix(0)
        if (compareDouble(1, firstRow(0)) && compareDouble(0, firstRow(1))) {
            // format: t + 0*s = c

            t = firstRow(2)
        }
        else { throw new IllegalStateException(s"(3) the equation solver sucks, got ${matrix.mkString("\n")}") } 

        val p1 = point(t)
        val p2 = l.point(-s)

        assert(p1 == p2, s"point no equal, $p1, $p2")

        Some(p1)
    }
}
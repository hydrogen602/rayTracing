package rayTracing.data

import org.junit._
import org.junit.Assert._

class TestLines {
    val ls1 = Line(Vect3(0, 0, 0), Vect3(1, 0, 0))
    val ls2 = Line(Vect3(4, 0, 0), Vect3(2, 2, 4))

    @Before def prepTest {

    }

    @Test def testIntersect {
        assertEquals(None, ls2.intersection(ls1))
    }
}
package rayTracing.data

import org.junit._
import org.junit.Assert._

class TestLightSource {
    val ls = LightSource(Vect3(10, 10, 10), DColor(255, 100, 10))

    @Before def prepTest {

    }

    @Test def testColorLightInit {
        assertTrue(DColor(255, 100, 10) == ls.color)
        assertTrue(Vect3(10, 10, 10) == ls.point)
    }

    @Test def testColorApply {
        val d = ls.applyColor(DColor(255, 255, 255))
        assertNotEquals(d, null)
        assertTrue(DColor(255, 100, 10) == d)
    }
}
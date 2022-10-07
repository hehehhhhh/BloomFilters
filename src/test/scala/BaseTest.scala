import org.junit.Test

class BaseTest {
    @Test
    def baseTest = {
        val array = Array(1,2,3,4,5,6,7,8,9)
        val pos = Array(1,3,6,7)

        println(pos.map(array).mkString(", "))
    }
}

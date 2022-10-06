import org.junit.Test

import scala.collection.mutable

class BloomFilterTest {
    @Test
    def bloomFilterTest(): Unit = {
        val bloomFilter = new BloomFilter[String](64, 0.05)

        bloomFilter.add("hello")
        bloomFilter.add("nihao")

        println(bloomFilter.contains("hello"))
        println(bloomFilter.contains("nihaoma"))
        println(bloomFilter.containsAll(Array("nihao", "hello")))
        println(bloomFilter.toString)
    }
}

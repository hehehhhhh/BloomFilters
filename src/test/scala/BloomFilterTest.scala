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

    @Test
    def countBloomFilterTest(): Unit = {
        val cbf = new CountBloomFilter[String](64, 0.05)

        cbf.addAll(Array("hello", "hello", "nihao", "atguigu"))

        println(cbf.contains("atguigu"))
        println(cbf.count("hello"))
        println(cbf.contains("nihaoya"))
        println(cbf.remove("atguigu"))
        println(cbf.contains("atguigu"))
        println(cbf.remove("atguigu"))

        println(cbf.toString)
    }
}

import java.nio.charset.Charset
import java.security.MessageDigest
import scala.collection.mutable

class CountBloomFilter[E](sizeOfBitSet: Int,
                          numberOfHashes: Int,
                          exceptedNumberOfElements: Int
                         )
    extends BloomFilter[E](sizeOfBitSet: Int,
    numberOfHashes: Int,
    exceptedNumberOfElements: Int){

    protected var counter = new Array[Byte](sizeOfBitSet)

    /**
     * Construct an empty Count Bloom Filter from the given number
     * of bits per element, excepted number of elements and the
     * number of hash functions.
     *
     * @param numberOfBitsPerElement number of bits per element
     * @param exceptedNumberOfElements excepted number of elements
     * @param numberOfHashes number of hash functions
     */
    def this(numberOfBitsPerElement: Double, exceptedNumberOfElements: Int, numberOfHashes: Int) = {
        this(Math.ceil(numberOfBitsPerElement * exceptedNumberOfElements).toInt,
            numberOfHashes,
            exceptedNumberOfElements
        )
    }

    /**
     * Construct an empty Count Bloom Filter from the given excepted
     * number of elements and false positive probability. The
     * number of hash functions is estimate to match the false
     * positive probability.
     *
     * @param exceptedNumberOfElements excepted number of elements in this Bloom filter
     * @param exceptedFalsePositiveProbability excepted false positive probability
     */
    def this(exceptedNumberOfElements: Int, exceptedFalsePositiveProbability: Double) = {
        this(Math.ceil(-(Math.log(exceptedFalsePositiveProbability) / Math.log(2))) / Math.log(2),
            exceptedNumberOfElements,
            Math.ceil(-(Math.log(exceptedFalsePositiveProbability) / Math.log(2))).toInt
        )
    }

    /**
     * Construct an new Bloom filter from a existing Bloom Filter data
     *
     * @param sizeOfBitSet size of BitSet
     * @param numberOfHashes number of hash functions
     * @param otherCountBloomFilter another bloom filter
     */
    def this(sizeOfBitSet: Int, numberOfHashes: Int, otherCountBloomFilter: CountBloomFilter[E]) = {
        this(sizeOfBitSet,
            numberOfHashes,
            otherCountBloomFilter.getExceptedNumberOfElements
        )
        
        this.bitSet = otherCountBloomFilter.getBitSet
        this.counter = otherCountBloomFilter.getCounter
    }

    /**
     * Get the counter of this Count Bloom Filter.
     *
     * @return counter of this Count Bloom Filter
     */
    def getCounter: Array[Byte] = {
        counter
    }

    /**
     * Reads a single bit on the counter.
     *
     * @param bit the bit to read.
     * @return count of this position.
     */
    def getCount(bit: Int): Byte = {
        counter(bit)
    }

    override def clear(): Unit = {
        super.clear()
        counter.map(x => 0)
    }

    /**
     * Add an element to this Count Bloom Filter. In addition, the count
     * of this position on counter will be increased by 1.
     *
     * @param bytes array of bytes to add to this Bloom Filter.
     * @return true if any position of BitSet has been changed.
     */
    override
    def add(bytes: Array[Byte]): Boolean = {
        val hashes = createHashes(bytes)

        hashes.foreach(x => {
            val index = Math.abs(x % sizeOfBitSet)
            bitSet.update(index, true)
            counter.update(index, (counter(index) + 1).toByte)
        })

        numberOfAddedElements += 1

        true
    }

    /**
     * Count the desired element in this Count Bloom Filter.
     *
     * @param element desired element
     * @return count of the element
     */
    def count(element: E): Int = {
        count(element.toString.getBytes(charset))
    }

    /**
     * Count the bytes in this Count Bloom Filter.
     *
     * @param bytes Array of Bytes
     * @return the min value of the position of bytes on the counter
     */
    def count(bytes: Array[Byte]): Int = {
        val hashes = createHashes(bytes)

        hashes.map(x => counter(Math.abs(x % sizeOfBitSet))).min
    }

    /**
     * Remove an element in this Count Bloom FIlter. It may incorrectly
     * remove some other elements.
     *
     * @param element element to remove
     * @return true if the element has been inserted
     */
    def remove (element: E): Boolean = {
        remove(element.toString.getBytes(charset))
    }

    /**
     * Remove an array of bytes from the Count Bloom Filter.
     *
     * @param bytes Array of byte
     * @return true if the bytes in this Count Bloom Filter
     */
    def remove (bytes: Array[Byte]): Boolean = {
        if (!contains(bytes)) {
            return false
        }

        val hashes = createHashes(bytes)

        hashes.foreach(x => {
            val index = Math.abs(x % sizeOfBitSet)
            counter.update(index, (counter(index) - 1).toByte)

            if (counter(index) == 0) {
                bitSet.update(index, false)
            }
        })

        true
    }
}

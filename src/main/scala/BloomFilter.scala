import java.nio.charset.Charset
import java.security.MessageDigest
import scala.collection.mutable
import scala.collection.mutable.BitSet.fromSpecific


class BloomFilter[E] (sizeOfBitSet: Int,
                   numberOfHashes: Int,
                   exceptedNumberOfElements: Int
                   ) extends Serializable{

    protected var bitSet = new mutable.BitSet(sizeOfBitSet)
    protected var numberOfAddedElements = 0
    protected var digestFunction: MessageDigest = java.security.MessageDigest.getInstance("MD5")
    protected var charset: Charset = Charset.forName("UTF-8")

    /**
     * Construct an empty Bloom Filter from the given number
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
     * Construct an empty Bloom Filter from the given excepted
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
     * @param otherBloomFilter another bloom filter
     */
    def this(sizeOfBitSet: Int, numberOfHashes: Int, otherBloomFilter: BloomFilter[E]) = {
        this(sizeOfBitSet,
            numberOfHashes,
            otherBloomFilter.getExceptedNumberOfElements
        )

        this.bitSet = otherBloomFilter.getBitSet
    }

    /**
     * Get the size of BitSet in this Bloom Filter
     *
     * @return size of BitSet
     */
    def getSizeOfBitSet: Int = {
        sizeOfBitSet
    }

    /**
     * Get the number of hash functions in this Bloom Filter
     *
     * @return number of hash functions
     */
    def getNumberOfHashes: Int = {
        numberOfHashes
    }

    /**
     * Get the excepted number of elements of this Bloom Filter
     *
     * @return excepted number of elements
     */
    def getExceptedNumberOfElements: Int = {
        exceptedNumberOfElements
    }

    /**
     * Get the BitSet used in this Bloom Filter
     *
     * @return BitSet
     */
    def getBitSet: mutable.BitSet = {
        bitSet
    }

    /**
     * Get the number of elements already in this Bloom Filter
     *
     * @return number of added elements
     */
    def getNumberOfAddedElement: Int = {
        numberOfAddedElements
    }

    /**
     * Read a single bit from this Bloom Filter.
     *
     * @param bit the bit to read.
     * @return true if the bit is set, false if it is not.
     */
    def getBit(bit: Int): Boolean = {
        bitSet(bit)
    }

    /**
     * Set the the digest algorithm.
     * Default is MD5, change to SHA-1 if necessary.
     *
     * @param algorithm digest algorithm
     */
    def setDigestFunction(algorithm: String = "MD5"): Unit = {
        digestFunction = java.security.MessageDigest.getInstance(algorithm)
    }

    /**
     * Set the charset of this Bloom Filter.
     * Default is UTF-8, change it if necessary.
     *
     * @param charsetName name of charset
     */
    def setCharSet(charsetName: String = "UTF-8"): Unit = {
        charset = Charset.forName(charsetName)
    }

    /**
     * Calculate the expected probability of false positives based on the
     * expected number of elements and the size of BitSet of this Bloom Filter.
     * <br /><br />
     * The value returned by this method is the <i>expected</i> probability
     * of false positives, assuming the number of added elements equals the
     * expected number of elements. If the number of added elements in the Bloom
     * Filter is less than the expected value, the true probability of false positives
     * will be lower.
     *
     * @return expected probability of false positives.
     */
    def exceptedFalsePositiveProbability: Double = {
        getFalsePositiveProbability(exceptedNumberOfElements)
    }

    /**
     * Calculate the current probability of false positives based on the
     * number of added elements and the size of BitSet of this Bloom Filter.
     *
     * @return probability of false positives.
     */
    def currentFalsePositiveProbability: Double = {
        getFalsePositiveProbability(numberOfAddedElements)
    }

    /**
     * Calculate the probability of a false positive given the specified
     * number of elements.
     *
     * @param number number of elements.
     * @return probability of a false positive.
     */
    def getFalsePositiveProbability(number: Int): Double = {
        Math.pow(1 - Math.exp(-numberOfHashes * number.toDouble / sizeOfBitSet.toDouble), numberOfHashes)
    }

    /**
     * Set all bits to false in this Bloom Filter.
     */
    def clear(): Unit = {
        bitSet.clear()
        numberOfAddedElements = 0
    }

    /**
     * Generate digests based on the contents of an array of bytes and
     * splits the result into 4-byte Int's and store them in an array. The
     * digest function is called until the required number of Int's are
     * produced. For each call to digest a salt is prepended to the data.
     * The salt is increased by 1 for each call.
     *
     * @param data specifies input data.
     * @return array of int-sized hashes
     */
    def createHashes(data: Array[Byte]): Array[Int] = {
        val result = new Array[Int](numberOfHashes)

        var salt = 0
        var k = 0
        while (k < numberOfHashes) {
            var digest: Array[Byte] = null

            digestFunction.synchronized{
                digestFunction.update(salt.toByte)

                salt += 1

                digest = digestFunction.digest(data)
            }

            var i = 0
            while (i < digest.length / 4 && k < numberOfHashes) {
                var h = 0

                for (j <- (i * 4) until (i * 4 + 4)) {
                    h <<= 4
                    h |= digest(j).toInt & 0xFF
                }

                result(k) = h
                k += 1
                i += 1
            }
        }

        result
    }

    /**
     * Add an element to this Bloom Filter. The output from the element's
     * toString() method is used as input of the hash functions.
     *
     * @param element an registered element of this Bloom Filter.
     * @return true if the element has been successfully inserted
     */
    def add(element: E): Boolean = {
        add(element.toString.getBytes(charset))
    }

    /**
     * Add an array of bytes to this Bloom Filter.
     *
     * @param bytes array of bytes to add to this Bloom Filter.
     * @return true if any position of BitSet has been changed.
     */
    def add(bytes: Array[Byte]): Boolean = {
        val hashes = createHashes(bytes)
        var flag = false

        hashes.foreach(x => {
            val index = Math.abs(x % sizeOfBitSet)
            flag |= !bitSet(index)
            bitSet.update(index, true)
        })

        numberOfAddedElements += 1

        flag
    }

    /**
     * Adds all elements from a Array to this Bloom Filter.
     *
     * @param elements Array of elements.
     */
    def addAll(elements: Iterable[_ <: E]): Unit = {
        elements.foreach(add)
    }

    /**
     * Returns true if the element has been inserted into this Bloom Filter.
     * See currentFalsePositiveProbability to calculate the probability of this
     * being correct.
     *
     * @param element element to check.
     * @return true if the element has been inserted into this Bloom Filter.
     */
    def contains(element: E): Boolean = {
        contains(element.toString.getBytes(charset))
    }

    /**
     * Returns true if the array of bytes has been inserted into this Bloom Filter.
     * See currentFalsePositiveProbability to calculate the probability of this
     * being correct.
     *
     * @param bytes Array of bytes to check.
     * @return true if the array has been inserted into the Bloom filter.
     */
    def contains(bytes: Array[Byte]): Boolean = {
        val hashes = createHashes(bytes)

        hashes.map(x => bitSet(Math.abs(x % sizeOfBitSet))).reduce(_ & _)
    }

    /**
     * Returns true if all the elements of a Array have been inserted into this
     * Bloom filter. See currentFalsePositiveProbability to calculate the
     * probability of this being correct.
     *
     * @param elements elements to check.
     * @return true if all the elements in c could have been inserted into the Bloom filter.
     */
    def containsAll(elements: Iterable[_ <: E]): Boolean = {
        elements.map(contains).reduce(_ & _)
    }

    override def toString: String = {
        s"""
          |$getClass: {
          | size of BitSet : $sizeOfBitSet
          | number of hashes : $numberOfHashes
          | excepted number of elements : $exceptedNumberOfElements
          | number of added elements : $numberOfAddedElements
          | excepted false positive probability : $exceptedFalsePositiveProbability
          | current false positive probability : $currentFalsePositiveProbability
          |}
          | """.stripMargin
    }
}
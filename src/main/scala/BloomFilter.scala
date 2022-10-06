import java.nio.charset.Charset
import java.security.MessageDigest

class BloomFilter[E] (sizeOfBitArray: Int,
                   numberOfHashes: Int,
                   exceptedNumberOfElements: Int,
                   bitArray: Array[Boolean]) {

    private var numberOfAddedElements = 0
    private val digestFunction: MessageDigest = java.security.MessageDigest.getInstance("MD5")
    private val charset = Charset.forName("UTF-8")

    /**
     * Constructs an empty Bloom filter from the given BitArray
     * size and the number of hash functions.
     *
     * @param sizeOfBitArray size of BitArray
     * @param numberOfHashes number of hash functions
     * @param exceptedNumberOfElements excepted number of element
     */
    def this(sizeOfBitArray: Int, numberOfHashes: Int, exceptedNumberOfElements: Int) = {
        this(sizeOfBitArray, numberOfHashes, exceptedNumberOfElements, new Array[Boolean](sizeOfBitArray))
    }

    private def this(numberOfBitsPerElement: Double, exceptedNumberOfElements: Int, numberOfHashes: Int) = {
        this(Math.ceil(numberOfBitsPerElement * exceptedNumberOfElements).toInt,
            numberOfHashes,
            exceptedNumberOfElements
        )
    }

    /**
     * Constructs an empty Bloom filter from the given excepted
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
     * Constructs an new Bloom filter from a existing Bloom Filter data
     *
     * @param sizeOfBitArray size of BitArray
     * @param numberOfHashes number of hash functions
     * @param otherBloomFilter another bloom filter
     */
    def this(sizeOfBitArray: Int, numberOfHashes: Int, otherBloomFilter: BloomFilter[E]) = {
        this(sizeOfBitArray,
            numberOfHashes,
            otherBloomFilter.getExceptedNumberOfElements,
            otherBloomFilter.getBitArray.clone()
        )
    }

    /**
     * get the size of BitArray in this Bloom Filter
     *
     * @return size of BitArray
     */
    def getSizeOfBitArray: Int = {
        sizeOfBitArray
    }

    /**
     * get the number of hash functions in this Bloom Filter
     *
     * @return number of hash functions
     */
    def getNumberOfHashes: Int = {
        numberOfHashes
    }

    /**
     * get the excepted number of elements of this Bloom Filter
     *
     * @return excepted number of elements
     */
    def getExceptedNumberOfElements: Int = {
        exceptedNumberOfElements
    }

    /**
     * get the BitArray used in this Bloom Filter
     *
     * @return BitArray
     */
    def getBitArray: Array[Boolean] = {
        bitArray
    }

    /**
     * get the number of elements already in this Bloom Filter
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
        bitArray(bit)
    }

    /**
     * Set a single bit in this Bloom Filter.
     *
     * @param bit   is the bit to set.
     * @param value If true, the bit is set. If false, the bit is cleared.
     */
    def setBit(bit: Int, value: Boolean): Unit = {
        bitArray.update(bit, value)
    }

    /**
     * Calculates the expected probability of false positives based on the
     * expected number of elements and the size of BitArray of this Bloom Filter.
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
     * Calculates the current probability of false positives based on the
     * number of added elements and the size of BitArray of this Bloom Filter.
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
        Math.pow(1 - Math.exp(-numberOfHashes * number.toDouble / sizeOfBitArray.toDouble), numberOfHashes)
    }

    /**
     * Sets all bits to false in this Bloom Filter.
     */
    def clear(): Unit = {
        bitArray.map(_ => false)
        numberOfAddedElements = 0
    }

    /**
     * Generates digests based on the contents of an array of bytes and
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
     * Adds an element to this Bloom Filter. The output from the element's
     * toString() method is used as input of the hash functions.
     *
     * @param element an registered element of this Bloom Filter.
     */
    def add(element: E): Unit = {
        add(element.toString.getBytes(charset))
    }

    /**
     * Adds an array of bytes to this Bloom Filter.
     *
     * @param bytes array of bytes to add to this Bloom Filter.
     */
    def add(bytes: Array[Byte]): Unit = {
        val hashes = createHashes(bytes)

        for (hash <- hashes) {
            bitArray.update(Math.abs(hash % sizeOfBitArray), true)
        }

        numberOfAddedElements += 1
    }

    /**
     * Adds all elements from a Array to this Bloom Filter.
     *
     * @param elements Array of elements.
     */
    def addAll(elements: Array[_ <: E]): Unit = {
        for (element <- elements) {
            add(element)
        }
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
     * @param bytes array of bytes to check.
     * @return true if the array has been inserted into the Bloom filter.
     */
    def contains(bytes: Array[Byte]): Boolean = {
        val hashes = createHashes(bytes)

        for (hash <- hashes) {
            if (!bitArray(Math.abs(hash % sizeOfBitArray))) {
                return false
            }
        }

        true
    }

    /**
     * Returns true if all the elements of a Array have been inserted into this
     * Bloom filter. See currentFalsePositiveProbability to calculate the
     * probability of this being correct.
     *
     * @param elements elements to check.
     * @return true if all the elements in c could have been inserted into the Bloom filter.
     */
    def containsAll(elements: Array[_ <: E]): Boolean = {
        for (element <- elements) {
            if (!contains(element)) {
                return false
            }
        }

        true
    }

    override def toString: String = {
        s"""
          |BloomFilter: {
          | size of BitArray : $sizeOfBitArray
          | number of hashes : $numberOfHashes
          | excepted number of elements : $exceptedNumberOfElements
          | number of added elements : $numberOfAddedElements
          | excepted false positive probability : $exceptedFalsePositiveProbability
          | current false positive probability : $currentFalsePositiveProbability
          |}
          | """.stripMargin
    }
}
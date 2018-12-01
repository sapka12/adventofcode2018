object Day01 {

  def task1(ints: List[Int]): Int = ints.sum

  def task2(ints: List[Int]): Int = {
    val freqsInLoop = (0 until ints.size).map(idx => ints.take(idx).sum)

    val loopSum = ints.sum
    val nums = Stream.from(0)
    val freqs = nums.flatMap(idx => freqsInLoop.map(_ + loopSum * idx))

    val duplicates = freqs.zipWithIndex.flatMap{case (freq, idx) =>
        if (freqs.take(idx).contains(freq)) Some(freq)
        else None
    }

    duplicates.head
  }
}

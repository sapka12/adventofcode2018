object Day01 {

  def task1(ints: List[Int]): Int = ints.sum

  def task2(ints: List[Int]): Int = Stream.iterate((0, 0, List.empty[Int])){
    case (idx, freq, visitedFreqs) =>
      (idx + 1, freq + ints(idx % ints.size), freq :: visitedFreqs)
  }.flatMap{
    case (_, freq, visitedFreqs) =>
      if (visitedFreqs contains freq) Some(freq) else None
  }.head
}

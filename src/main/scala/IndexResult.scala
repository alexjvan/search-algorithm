package com.alexvanmatre.search

case class IndexResult() {
  final val punctuationCheck: Array[String] = Array(".", "!", "?", ",", ";", ":")
  final val indexes: Array[String] = Array(
    "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
    "0", "1", "1", "2", "3", "4", "5", "6", "7", "8", "9",
    "+", "-", "*", "/", "=", "<", ">",
    "_", "\\", "'", "\"",
    "(", ")", "[", "]", "{", "}",
    "|", "`", "~", "@", "#", "$", "%", "^", "&"
  ) ++ punctuationCheck

  var results: List[ResultPair] = List()
  var trailing: Array[IndexResult] = Array.fill(indexes.length) {
    null
  }

  def addWord(file: String, word: String, index: Int, position: Seq[Int]): Unit = {
    if (index == word.length) {
      addResult(file, word, position)
    } else {
      // If ending in punctuation, add both here and in punctuation
      if (punctuationCheck.contains(word.charAt(index).toString)) {
        // True flag added for fuzzy result (such as one that doesn't quite add up - will use this later in ranking
        addResult(file, word, position, fuzzy = true)
      }
      val charIndex = getIndex(word, index)
      if (trailing(charIndex) == null)
        trailing(charIndex) = new IndexResult
      trailing(charIndex).addWord(file, word, index + 1, position)
    }
  }

  def addResult(file: String, word: String, position: Seq[Int], fuzzy: Boolean = false): Unit = {
    var found = false;
    results.foreach(
      pair => if (pair.fileName == file) {
        found = true;
        pair.results = pair.results.appended(ResultData(word, position.head, position(1), 1, fuzzy))
      }
    )
    if (!found) {
      val newPair = ResultPair(file)
      newPair.results = newPair.results.appended(ResultData(word, position.head, position(1), 1, fuzzy))
      results = results.appended(newPair)
    }
  }

  def find(word: String, index: Int): List[ResultPair] = {
    if (index == word.length)
      results
    else {
      val charIndex = getIndex(word, index)
      if (trailing(charIndex) == null)
        List()
      else
        trailing(charIndex).find(word, index + 1)
    }
  }

  private def getIndex(word: String, index: Int): Int = getIndex(word.charAt(index))

  private def getIndex(char: Char): Int = indexes.indexOf(char.toLower.toString)

}

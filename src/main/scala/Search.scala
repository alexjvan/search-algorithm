package com.alexvanmatre.search

object Search {
  final val indexLocations: List[String] = List(
    "files/"
  )
  var indexer: Indexer = new Indexer
  var ranker: Ranker = new Ranker
  var fileCount: Int = 0


  def main(args: Array[String]): Unit = {
    if(args.length == 0) {
      println("Must include at least one word to index")
      return
    }

    var searchTerm: String = ""
    args.foreach(arg => searchTerm = searchTerm + arg + " ")
    searchTerm = searchTerm.substring(0, searchTerm.length - 1)

    // List of times in milliseconds
    var times: List[Long] = List()
    var time1: Long = System.nanoTime()/1000
    indexer.store(indexLocations)
    var time2: Long = System.nanoTime()/1000
    times = times.appended(time2 - time1)
    time1 = time2
    val results = indexer.find(searchTerm)
    time2 = System.nanoTime()/1000
    times = times.appended(time2 - time1)
    time1 = time2
    val rankings = ranker.rank(results, searchTerm)
    time2 = System.nanoTime()/1000
    times = times.appended(time2 - time1)
    output(rankings, times)
  }

  def output(results: List[ResultPair], times: List[Long]): Unit = {
    seperatorLine()
    println("\nResults: ")

    if(results.nonEmpty){
      var i = 1
      results.foreach(res => {
        print(s"[$i] ")
        println(res)
        i += 1
      })
    } else println("No results")
    seperatorLine()
    println(s"\nFile Count: ${fileCount}")
    println(s"Indexing Files: ${times.head/1000}s")
    println(s"Search: ${times(1)/1000}s")
    println(s"Ranking: ${times(2)/1000}s")
  }

  private def seperatorLine(): Unit =
    0.to(25).foreach(_ => print("-"))
}

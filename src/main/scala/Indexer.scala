package com.alexvanmatre.search

import java.io.File
import scala.io.Source

class Indexer {

  final val root: IndexResult = new IndexResult

  def store(locations: List[String]): Int = {
    var fileCount = 0
    var files: List[File] = List()
    locations.foreach(loc => files = files.appendedAll(getListOfFiles(loc)))
    fileCount = files.length
    val position: Array[Int] = Array(1, 1)
    files.foreach(
      file => {
        val source = Source.fromFile(file)
        source.getLines.foreach(
          line =>  {
            line.split(" ").foreach(
              word => {
                addWord(file.getName, word, position)
                position(1) += 1
              } // end word
            )
            position(0) += 1
            position(1) = 1
          } // end line
        )
        source.close()
        position(0) = 1
        position(1) = 1
      } // end file
    )
    fileCount
  }

  private def getListOfFiles(dir: String): List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory)
      d.listFiles.filter(_.isFile).toList
    else if(d.exists() && d.isFile)
      List(d)
    else
      List[File]()
  }

  def addWord(file: String, word: String, position: Seq[Int]): Unit = root.addWord(file, word, 0, position)

  def find(word: String): List[ResultPair] = {
    val split = word.split(" ").filterNot(value => value == "" || value == " ")
    if(split.length == 1)
      root.find(word, 0)
    else {
      var results: List[List[ResultPair]] = List()
      split.foreach(word => results = results.appended(root.find(word, 0)))
      val combined = combineResults(results)
      smashResults(combined)
    }
  }

//  def combineResults(results: List[List[ResultPair]]): List[ResultPair] = {
//    val ret = results.head
//
//    1.to(results.length).foreach(
//      num => results(num).foreach(
//        resultSet => {
//          if(ret.exists(_.fileName == resultSet.fileName)) {
//            var found = ret.filter(_.fileName == resultSet.fileName).head
//            resultSet.results.foreach(
//              result => {
//                var set: Boolean = false
//                found.results.foreach(
//                  internal => {
//                    if(internal.lineCount == result.lineCount) {
//                      // <res> <internal _>
//                      if(result.wordCount + 1 == internal.wordCount) {
//                        internal.wordSpan += 1
//                        internal.term += " " + result.term
//                        if(result.fuzzy)
//                          internal.fuzzy = true
//                        set = true
//                      }
//                      // <internal _> <res>
//                      else if(result.wordCount == internal.wordCount + internal.wordSpan + 1) {
//                        internal.wordSpan += 1
//                        internal.term = result.term + " " + internal.term
//                        if(result.fuzzy)
//                          internal.fuzzy = true
//                        set = true;
//                      }
//                    }
//                  } // End internal
//                )
//                if(!set) {
//                  found.results.appended(result)
//                }
//              } // End Result
//            )
//          } else {
//            ret.appended(resultSet)
//          }
//        } // End result
//      )
//    )
//    ret
//  }

  def combineResults(value: List[List[ResultPair]]): List[ResultPair] = {
    var ret = value.head

    1.until(value.length).foreach(
      num => value(num).foreach(
        resultSet => if(ret.exists(_.fileName == resultSet.fileName)) {
          val found = ret.filter(_.fileName == resultSet.fileName).head
          resultSet.results.foreach(
            result => found.results = found.results.appended(result)
          )
        } else
          ret = ret.appended(resultSet)
      )
    )

    ret
  }

  def smashResults(value: List[ResultPair]): List[ResultPair] = {
    value.map(
      pair => {
        val i = 0
        i.until(pair.results.length).foreach(
          _ => {
            var ires = pair.results(i)
            var j = i + 1
            j.until(pair.results.length).foreach(
              _ => {
                val jres = pair.results(j)
                if(ires.lineCount == jres.lineCount) {
                  if(ires.wordCount + ires.wordSpan == jres.wordCount) {
                    ires.wordSpan += jres.wordSpan
                    ires.term += " " + jres.term
                    if(jres.fuzzy)
                      ires.fuzzy = true
                    j -= 1
                    pair.results = pair.results.filterNot(_ == jres)
                  } else if(jres.wordCount + jres.wordSpan == ires.wordCount) {
                    jres.wordSpan += ires.wordSpan
                    jres.term += " " + ires.term
                    if(ires.fuzzy)
                      jres.fuzzy = true;
                    j -= 1
                    pair.results = pair.results.filterNot(_ == ires)
                    // Reset ires as we just removed ires
                    ires = pair.results(1)
                  }
                }
              }
            )
          }
        )
        pair
      }
    )
  }

}

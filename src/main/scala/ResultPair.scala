package com.alexvanmatre.search

case class ResultPair(
 var fileName: String
) {
  var results: List[ResultData] = List()

  override def toString: String = {
    var ret = s"File: ${fileName}, Results: ${results.length}"
    results.foreach(res => ret += "\n" + res.toString)
    ret
  }
}

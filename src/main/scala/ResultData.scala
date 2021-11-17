package com.alexvanmatre.search

case class ResultData(
   var term: String,
   var lineCount: Int,
   var wordCount: Int,
   var wordSpan: Int ,
   var fuzzy: Boolean
 ) {
  override def toString: String = {
    var ret = s"${term} - Line: ${lineCount}, Word: ${wordCount}"
    if (fuzzy)
      ret += ", Fuzzy: true"
    ret
  }
}

package com.alexvanmatre.search

class Ranker {

  private final val fuzzyResultValue = 1
  private final val fixedResultValue = 2
  private final val concurrentResults = 5
  private final val titleResultValue = 5

  def rank(values: List[ResultPair], searchTerm: String): List[ResultPair] = {
    if(values.length <= 1)
      return values
    var ranked: Map[ResultPair, Int] = Map()
    var setValue = 0
    values.foreach(
      value => {
        value.results.foreach(
          result => {
            if(result.fuzzy)
              setValue += (fixedResultValue - fuzzyResultValue)*concurrentResults
            else
              setValue += fixedResultValue*concurrentResults
          } // End result
        )
        if(value.fileName.contains(searchTerm))
          setValue += titleResultValue

        ranked = ranked + (value -> setValue)
      } // End value
    )
    ranked.toList.sortBy(_._2).reverse.toMap.keys.toList
  }

}

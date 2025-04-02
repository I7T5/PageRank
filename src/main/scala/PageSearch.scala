import scala.math.log
//import scala.collection.parallel.CollectionConverters._

object PageSearch {
    /**
     * @param pages  a list of RankedWebPage objects to be searched
     * @param query  a list of search terms to be counted in those pages
     * @return       a list of the number of times any of the terms appeared in each page in the same order as given
     */
    def count(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        for page <- pages yield (for (s <- query if !s.isBlank) yield page.text.sliding(s.length).count(window => window == s)).sum
    }

    /**
     * @param pages a list of RankedWebPage objects to be searched
     * @param query a list of search terms to be counted in those pages
     * @return      a list of the term-frequency of the occurrences of those terms in each page in the same order given
     */
    def tf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        val cs = count(pages,query)
        (for i <- pages.indices yield cs(i) / pages(i).text.length).toList
    }

    /**
     * @param pages a list of RankedWebPage objects to be searched
     * @param query a list of search terms to be counted in those pages
     * @return      a list of the TF-IDF score for each page in the same order given
     */
    def tfidf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        // find inverse freq for query
        val IDF = (for s <- query yield log(pages.length.toDouble / (pages.count(p => p.text.contains(s)) + 1))).sum

        // find tf for each term
        val tfScores = tf(pages,query)
        
        tfScores.map(_*IDF)
    }
}
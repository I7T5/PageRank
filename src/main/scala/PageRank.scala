import scala.util.Random
//import scala.collection.parallel.CollectionConverters._

object PageRank {
    /**
     * @param pages A map of page.id to page for some number of WebPage objects
     * @return      A map of page.id to a weight of 1.0 for those same WebPage objects
     */
    def equal(pages: Map[String, WebPage]): Map[String, Double] = {
        var tbl = Map[String, Double]()
        for (str <- pages.keys) do tbl += (str -> 1.0)
        tbl
    }

    /**
     * @param pages A map of page.id to page for some number of WebPage objects
     * @return A map of page.id to a weight that is a simple count of the number of pages linking to that page
     */
    def indegree(pages: Map[String, WebPage]): Map[String, Double] = {
        pages.map((id: String, _) => id -> pages.values.count(_.links.toSet.contains(id)).toDouble)  // NOTE: links: List[id] not List[urls]
    }

    def pagerank(pages: Map[String, WebPage]): Map[String, Double] = {
        Map() // TODO: remove this stub and implement this method
    }
}
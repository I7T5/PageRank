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
        val numWalks: Int = 10000
        val stepsPerWalk: Int = 100
        var ranking: Map[String, Double] = pages.map((id, _) => id -> 0.0)  // initialize; could do getOrElse too ¯\_(ツ)_/¯

        // computation-saving constants
        val numPages: Int = pages.size
        val pageIds: List[String] = pages.keys.toList

        for w <- 0 until numWalks do {  // walk
            // NOTE: pages.keySet.head doesn't give a random item the same way the following line does...
            var currPage: String = pageIds(Random.nextInt(numPages))

            for s <- 0 until stepsPerWalk do {  // step
                val follow: Boolean = Random.nextDouble() < 0.85 && pages(currPage).links.nonEmpty
                if (follow) {
                    val links: List[String] = pages(currPage).links
                    currPage = links(Random.nextInt(links.size))
                } else {  // randomly jump to a page
                     currPage = pageIds(Random.nextInt(numPages))
                }

                // using ranking as storage for R_i aka numStops first
                ranking += (currPage -> (ranking(currPage) + 1))
            }
        }

        // update R_i values to W_i = (R_i + 1)/(S + N)
        ranking.map((id: String, numStops: Double) => id -> ((numStops+1) / (numWalks+stepsPerWalk)))
    }
}
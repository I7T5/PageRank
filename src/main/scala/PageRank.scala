import scala.annotation.tailrec
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

        // computation-saving constants
        val numPages: Int = pages.size
        val pageIds: List[String] = pages.keys.toList

        // simulating a random walker
        @tailrec
        def nextPage(traversedPages: List[String], step: Int): List[String] = {
            if (step == stepsPerWalk) return traversedPages
            val currPage: String = traversedPages.head
            val follow: Boolean = Random.nextDouble() < 0.85 && pages(currPage).links.nonEmpty
            val links: List[String] = pages(currPage).links
            val next: String = if follow then links(Random.nextInt(links.size)) else pageIds(Random.nextInt(numPages))
            nextPage(traversedPages :+ next, step + 1)
        }

        // a flattened list of all the walkers' path
        val stops: Seq[String] = (0 until numWalks).flatMap(_ => nextPage(List(pageIds(Random.nextInt(numPages))), 0))

        pageIds.map((id: String) => id -> (1.0 * (stops.count(_ == id)+1) / (numWalks+stepsPerWalk))).toMap
    }
}
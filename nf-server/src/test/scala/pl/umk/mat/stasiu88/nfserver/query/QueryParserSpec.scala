package pl.umk.mat.stasiu88.nfserver.query

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class QueryParserSpec extends FlatSpec with ShouldMatchers {
  val validSplitFilters = List(
    "all",
    "all[tcp]",
    "anyip == 127.0.0.1",
    "bothip in 192.168.0.0/16"
  )

  validSplitFilters foreach { query =>
    query should "be parsed" in {
      val parser = new QueryParser
      parser.parseAll(parser.splitfilter, query)
    }
  }
}
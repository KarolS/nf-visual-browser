package pl.umk.mat.stasiu88.nfserver.query

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import pl.umk.mat.stasiu88.nfserver._
import pl.umk.mat.stasiu88.nfserver.query._
import Protocols._

@RunWith(classOf[JUnitRunner])
class SplitFilterSpec extends FlatSpec with ShouldMatchers {
  val flow1 = new Flow(
    startTime = 0,
    endTime = 0,
    bytes = 10000,
    packets = 10,
    srcaddr = Addr("192.168.0.1"),
    destaddr = Addr("55.55.55.55"),
    srcport = 34200,
    destport = 80,
    protocol = TCP,
    nextHop = Addr("2.2.2.2"),
    tos = 0,
    tcpFlags = 0,
    srcAS = 0,
    destAS = 0,
    inputInterface = 0,
    outputInterface = 0
  )
  val flow2 = new Flow(
    startTime = 0,
    endTime = 0,
    bytes = 10000,
    packets = 10,
    srcaddr = Addr("192.168.0.2"),
    destaddr = Addr("55.55.55.55"),
    srcport = 34200,
    destport = 1111,
    protocol = UDP,
    nextHop = Addr("2.2.2.2"),
    tos = 0,
    tcpFlags = 0,
    srcAS = 0,
    destAS = 0,
    inputInterface = 0,
    outputInterface = 0
  )
  val flow3 = new Flow(
    startTime = 0,
    endTime = 0,
    bytes = 100,
    packets = 1,
    srcaddr = Addr("10.0.0.1"),
    destaddr = Addr("55.55.55.55"),
    srcport = 0,
    destport = 0,
    protocol = ICMP,
    nextHop = Addr("2.2.2.2"),
    tos = 0,
    tcpFlags = 0,
    srcAS = 0,
    destAS = 0,
    inputInterface = 0,
    outputInterface = 0
  )

  val subnet192 = Subnet(IP4Addr("192.168.0.0"), 16)

  val filter1 = NodeSplitFilter(
    AllFilter, List(
      LeafSplitFilter(ProtocolFilter(TCP)),
      LeafSplitFilter(IPMaskFilter(AnyIpDesignation, subnet192))
    )
  )
  val filter2 = NodeSplitFilter(
    AllFilter, List(
      NodeSplitFilter(
        ProtocolFilter(ICMP), List(
          LeafSplitFilter(NotFilter(IPMaskFilter(BothIpDesignation, subnet192))),
          LeafSplitFilter(AllFilter)
        )
      ),
      NodeSplitFilter(
        ProtocolFilter(TCP), List(
          LeafSplitFilter(IPMaskFilter(BothIpDesignation, subnet192)),
          LeafSplitFilter(AllFilter)
        )
      )
    )
  )

  filter1.bucketNo = 0
  filter2.bucketNo = 0

  val filter1x2 = filter1 * filter2
  val filter2x1 = filter2 * filter1

  "Filter" should "have correct size" in {
    filter1.bucketCount should equal (2)
    filter2.bucketCount should equal (4)
  }

  it should "classify correctly" in {
    filter1 classify flow1 should equal (Some(0))
    filter1 classify flow2 should equal (Some(1))
    filter1 classify flow3 should equal (None)
    filter2 classify flow1 should equal (Some(3))
    filter2 classify flow2 should equal (None)
    filter2 classify flow3 should equal (Some(0))
  }

  "Product filter" should "have correct size" in {
    filter1x2.bucketCount should equal (8)
    filter2x1.bucketCount should equal (8)
  }

  it should "classify correctly" in {
    filter1x2 classify flow1 should equal (Some(3))
    filter1x2 classify flow2 should equal (None)
    filter1x2 classify flow3 should equal (None)
    filter2x1 classify flow1 should equal (Some(6))
    filter2x1 classify flow2 should equal (None)
    filter2x1 classify flow3 should equal (None)
  }
}
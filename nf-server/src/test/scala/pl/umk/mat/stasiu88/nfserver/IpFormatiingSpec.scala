package pl.umk.mat.stasiu88.nfserver

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class IpFormatiingSpec extends FlatSpec with ShouldMatchers {
  "IPv4" should "be read from string" in {
    IP4Addr("127.0.0.1") should equal (IP4Addr(0x7f000001))
  }
  "IPv4" should "be converted to string" in {
    IP4Addr(0x7f000001).toString should equal ("127.0.0.1")
  }
  "IPv6" should "be read from string" in {
    IP6Addr("::1") should equal (IP6Addr(0,1))
  }
  "IPv6" should "be converted to string" in {
    IP6Addr(0,1).toString should equal ("::1")
    IP6Addr(0xfe80000000000000L,0x45674567L).toString should equal ("fe80::45674567")
  }
}
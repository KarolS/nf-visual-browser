package pl.umk.mat.stasiu88.nfserver.input

import java.io.ByteArrayInputStream
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class BigEndianByteStreamSpec extends FlatSpec with ShouldMatchers {
  val stream = new BigEndianByteStream(new ByteArrayInputStream(Array[Byte](8,16,0,32,0,0,0,64,0,0,0,0,0,0,0,1)))
  "A stream" should  "read a byte correctly" in {
	stream.get8() should equal (8)
  }
  it should  "read a word correctly" in {
	stream.get16() should equal (16)
  }
  it should  "read a dword correctly" in {
	stream.get32() should equal (32)
  }
  it should  "read a qword correctly" in {
	stream.get64() should equal (64)
  }
}

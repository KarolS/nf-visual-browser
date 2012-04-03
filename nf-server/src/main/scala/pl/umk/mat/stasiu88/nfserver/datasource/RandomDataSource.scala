package pl.umk.mat.stasiu88.nfserver.datasource

import pl.umk.mat.stasiu88.nfserver.IP4Addr
import scala.util.Random
import pl.umk.mat.stasiu88.nfserver.dummy.RandomFlowData
import pl.umk.mat.stasiu88.nfserver.worker.Result
import pl.umk.mat.stasiu88.nfserver.query.Query
import pl.umk.mat.stasiu88.nfserver.Flow
import pl.umk.mat.stasiu88.nfserver.worker.MutableResult

class RandomDataSource(size:Int) extends DataSource{
  private[this] val data = new RandomFlowData(
      localSubnet= IP4Addr("192.168.0.0"),
      subnetSize= 254,
      startTime= 1000000000L,
      measurementDuration= 10000000000L,
      maxFlowDuration= 1000,
      maxFlowPacketCount= 1000,
      maxBytesPerPacket= 1000,
      popularTcpPorts= List(22,80,443),
      random= new Random
      )
  def foreach(query:Query)(function:Flow=>Unit) {
    for(flow <- data.stream().take(size)){
      function(flow)
    }
  }
}
package pl.umk.mat.stasiu88.nfserver

object Protocols {
  val TCP = 6
  val UDP = 17
  val ICMP = 1
  def name(id: Int) = id match{
    case 6 => "tcp"
    case 17 => "udp"
    case 1 => "icmp"
    case x => "proto="+x 
  }
}
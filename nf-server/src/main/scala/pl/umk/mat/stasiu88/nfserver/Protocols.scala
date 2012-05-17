package pl.umk.mat.stasiu88.nfserver

object Protocols {
  val TCP = 6
  val UDP = 17
  val ICMP = 1
  def name(id: Int) = id match{
    case 6 => "TCP"
    case 17 => "UDP"
    case 1 => "ICMP"
    case x => "protocol "+x 
  }
}
package pl.umk.mat.stasiu88.nfserver

object Protocols {
  val TCP = 6
  val UDP = 17
  val ICMP = 1
  val IGMP = 2
  val EGP = 8
  val RSVP = 46
  val ICMP6 = 58
  val SCTP = 132
  def name(id: Int) = id match{
    case 6 => "TCP"
    case 17 => "UDP"
    case 1 => "ICMP"
    case 2 => "IGMP"
    case 8 => "EGP"
    case 46 => "RSVP"
    case 58 => "ICMP6"
    case 132 => "SCTP"
    case x => "protocol "+x 
  }
}

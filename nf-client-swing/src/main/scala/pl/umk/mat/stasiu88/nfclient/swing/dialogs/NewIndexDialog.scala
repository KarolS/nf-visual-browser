package pl.umk.mat.stasiu88.nfclient.swing.dialogs

import javax.swing._
import scalaz._
import Scalaz._
import pl.umk.mat.stasiu88.nfclient.SwingUtils._

class NewIndexDialog(parent: JFrame)
  extends NewListItemDialog(
    parent, 
    "New Index",
    
    ListItemBuilder("IP version", Seq(), "ipversion"),
    ListItemBuilder("Source IP", Seq(), "srcip"),
    ListItemBuilder("Destination IP", Seq(), "dstip"),
    ListItemBuilder("IP", Seq(), "ip"),
    ListItemBuilder("IP in subnet", Seq(new Param("Subnet:"){
      def parse(i:String) = ParamUtils.validateSubnet("Invalid subnet", i)
    }), "ip in ", ""),
    ListItemBuilder("IP not in subnet", Seq(new Param("Subnet:"){
      def parse(i:String) = ParamUtils.validateSubnet("Invalid subnet", i)
    }), "ip not in ", ""),
    ListItemBuilder("Source hostname", Seq(), "srchost"),
    ListItemBuilder("Destination hostname", Seq(), "dsthost"),
    ListItemBuilder("Hostname", Seq(), "host"),
    ListItemBuilder("Port", Seq(), "port"),
    ListItemBuilder("Source port", Seq(), "srcport"),
    ListItemBuilder("Destination port", Seq(), "dstport"),
    ListItemBuilder("Protocol", Seq(), "protocol") //TODO

  ){
  
}

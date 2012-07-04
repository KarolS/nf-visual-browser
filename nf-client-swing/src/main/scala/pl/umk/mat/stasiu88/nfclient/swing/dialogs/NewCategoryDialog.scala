/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfclient.swing.dialogs


import javax.swing._
import scalaz._
import Scalaz._
import pl.umk.mat.stasiu88.nfclient.SwingUtils._

class NewCategoryDialog(parent: JFrame)
  extends NewListItemDialog(
    parent, 
    "New category",
    
    ListItemBuilder("All flows", Seq(), "all"),
    ListItemBuilder("TCP flows", Seq(), "tcp"),
    ListItemBuilder("UDP flows", Seq(), "udp"),
    ListItemBuilder("ICMP flows", Seq(), "icmp"),
    
    ListItemBuilder("Source port equals", Seq(new Param("Source port:"){
      def parse(i:String) = ParamUtils.validatePort("Invalid port", i)
    }), "srcport = ", ""),
    ListItemBuilder("Destination port equals", Seq(new Param("Destination port:"){
      def parse(i:String) = ParamUtils.validatePort("Invalid port", i)
    }), "dstport = ", ""),
    ListItemBuilder("Any port equals", Seq(new Param("Port:"){
      def parse(i:String) = ParamUtils.validatePort("Invalid port", i)
    }), "port = ", ""),
    ListItemBuilder("Two ports in any direction ", Seq(new Param("Port 1:"){
      def parse(i:String) = ParamUtils.validatePort("Invalid port", i)
    }, new Param("Port 2:"){
      def parse(i:String) = ParamUtils.validatePort("Invalid port", i)
    }), "(anyport = ", " and anyport = ", ")"), //TODO OK?
    
    ListItemBuilder("Source port in range", Seq(new Param("Minimum port:"){
      def parse(i:String) = ParamUtils.validatePort("Invalid port", i)
    },new Param("Maximum port:"){
      def parse(i:String) = ParamUtils.validatePort("Invalid port", i)
    }), "srcport in ", "..", ""),
     ListItemBuilder("Destination port in range", Seq(new Param("Minimum port:"){
      def parse(i:String) = ParamUtils.validatePort("Invalid port", i)
    },new Param("Maximum port:"){
      def parse(i:String) = ParamUtils.validatePort("Invalid port", i)
    }), "dstport in ", "..", ""),
    ListItemBuilder("Any port in range", Seq(new Param("Minimum port:"){
      def parse(i:String) = ParamUtils.validatePort("Invalid port", i)
    },new Param("Maximum port:"){
      def parse(i:String) = ParamUtils.validatePort("Invalid port", i)
    }), "anyport in ", "..", ""),
    ListItemBuilder("Both ports in range", Seq(new Param("Minimum port:"){
      def parse(i:String) = ParamUtils.validatePort("Invalid port", i)
    },new Param("Maximum port:"){
      def parse(i:String) = ParamUtils.validatePort("Invalid port", i)
    }), "bothport in ", "..", ""),
    
    ListItemBuilder("Source IP equals", Seq(new Param("IP:"){
      def parse(i:String) = ParamUtils.validateIP("Invalid IP", i)
    }), "srcip = ", ""),
    ListItemBuilder("Destination IP equals", Seq(new Param("IP:"){
      def parse(i:String) = ParamUtils.validateIP("Invalid IP", i)
    }), "dstip = ", ""),
    ListItemBuilder("Any IP equals", Seq(new Param("IP:"){
      def parse(i:String) = ParamUtils.validateIP("Invalid IP", i)
    }), "anyip = ", ""),
    ListItemBuilder("Two IPs in any direction", Seq(new Param("IP 1:"){
      def parse(i:String) = ParamUtils.validateIP("Invalid IP", i)
    }, new Param("IP 2:"){
      def parse(i:String) = ParamUtils.validateIP("Invalid IP", i)
    }), "(anyip = ", " and anyip = ", ")"),
    
    ListItemBuilder("Source IP in subnetwork", Seq(new Param("Subnet:"){
      def parse(i:String) = ParamUtils.validateSubnet("Invalid subnet", i)
    }), "srcip in ", ""),
    ListItemBuilder("Destination IP in subnetwork", Seq(new Param("Subnet:"){
      def parse(i:String) = ParamUtils.validateSubnet("Invalid subnet", i)
    }), "dstip in ", ""),
    ListItemBuilder("Any IP in subnetwork", Seq(new Param("Subnet:"){
      def parse(i:String) = ParamUtils.validateSubnet("Invalid subnet", i)
    }), "anyip in ", ""),
    ListItemBuilder("Both IPs in subnetwork", Seq(new Param("Subnet:"){
      def parse(i:String) = ParamUtils.validateSubnet("Invalid subnet", i)
    }), "bothip in ", ""),
    
    ListItemBuilder("Source IP not in subnetwork", Seq(new Param("Subnet:"){
      def parse(i:String) = ParamUtils.validateSubnet("Invalid subnet", i)
    }), "srcip not in ", ""),
    ListItemBuilder("Destination IP not in subnetwork", Seq(new Param("Subnet:"){
      def parse(i:String) = ParamUtils.validateSubnet("Invalid subnet", i)
    }), "dstip not in ", ""),
    ListItemBuilder("Any IP not in subnetwork", Seq(new Param("Subnet:"){
      def parse(i:String) = ParamUtils.validateSubnet("Invalid subnet", i)
    }), "anyip not in ", ""),
    ListItemBuilder("Both IPs not in subnetwork", Seq(new Param("Subnet:"){
      def parse(i:String) = ParamUtils.validateSubnet("Invalid subnet", i)
    }), "bothip not in ", ""),
    
    ListItemBuilder("Input interface equals", Seq(new Param("Interface:"){
      def parse(i:String) = ParamUtils.validateInterface("Invalid interface", i)
    }), "inputif = ", ""),
    ListItemBuilder("Output interface equals", Seq(new Param("Interface:"){
      def parse(i:String) = ParamUtils.validateInterface("Invalid interface", i)
    }), "outputif = ", ""),
    ListItemBuilder("Any interface equals", Seq(new Param("Interface:"){
      def parse(i:String) = ParamUtils.validateInterface("Invalid interface", i)
    }), "anyif = ", ""),
    ListItemBuilder("No interface equals", Seq(new Param("Interface:"){
      def parse(i:String) = ParamUtils.validateInterface("Invalid interface", i)
    }), "bothif != ", ""),
    ListItemBuilder("Between two interfaces", Seq(new Param("Interface 1:"){
      def parse(i:String) = ParamUtils.validateInterface("Invalid interface", i)
    }, new Param("Interface 2:"){
      def parse(i:String) = ParamUtils.validateInterface("Invalid interface", i)
    }), "( anyif = ", " and anyif = ", ")"),
    
    ListItemBuilder("Type of Service", Seq(new Param("Mask:"){
      def parse(i:String) = ParamUtils.validateByte("Invalid mask", i)
    }, new Param("Masked ToS:"){
      def parse(i:String) = ParamUtils.validateByte("Invalid byte", i)
    }), "tos mask ", " = ", ""),
    
    ListItemBuilder("HTTP(s) traffic from inside to foreign servers", Seq(new Param("Local network:"){
      def parse(i:String) = ParamUtils.validateSubnet("Invalid subnet", i)
    }), { params =>
      val subnet = params(0)
      ( "(   ((srcport = 80 or srcport = 443) and srcip not in "
      + subnet
      + " and dstip in "
      + subnet
      + ") or ((dstport = 80 or dstport = 443) and dstip not in "
      + subnet
      + " and srcip in "
      + subnet
      + ")   )"
      )
    }),
    ListItemBuilder("HTTP(s) traffic from outside to local servers", Seq(new Param("Local network:"){
      def parse(i:String) = ParamUtils.validateSubnet("Invalid subnet", i)
    }), { params =>
      val subnet = params(0)
      ( "(   ((srcport = 80 or srcport = 443) and srcip in "
      + subnet
      + " and dstip not in "
      + subnet
      + ") or ((dstport = 80 or dstport = 443) and dstip in "
      + subnet
      + " and srcip not in "
      + subnet
      + ")   )"
      )
    }),
    ListItemBuilder("Local HTTP(s) traffic", Seq(new Param("Local network:"){
      def parse(i:String) = ParamUtils.validateSubnet("Invalid subnet", i)
    }), { params =>
      val subnet = params(0)
      ( "((anyport = 80 or anyport = 443) and bothip in "
      + subnet
      + ")"
      )
    }),
    
    
    ListItemBuilder("Custom category (advanced)", Seq(new Param("Expression:"){
      def parse(i:String) = i.success
    }), _.mkString(""))
  ){
  
}

package pl.umk.mat.stasiu88.nfclient.swing.dialogs

import javax.swing._
import scalaz._
import Scalaz._
import pl.umk.mat.stasiu88.nfclient.SwingUtils._
import pl.umk.mat.stasiu88.nfclient.SubnetCache


class ManageSubnetsDialog(parent:JFrame) extends ModalDialog[Unit](parent, "Manage subnetworks"){
  
  def readValue() = ().success
  def displayValue(v:Unit) {}
  
  val newSubnet = new NewSubnetDialog(parent)
  val subnetList = new JTable(SubnetCache)
  setSize(600,450)
  withLayout(this, "5dlu,p,5dlu,p,5dlu,p,5dlu:g,p,5dlu", "5dlu,f:0dlu:g,5dlu,p,5dlu").
  add(2,2, 7,1, scrollable(subnetList)).
  add(2,4, button("Add"){
    newSubnet.setValue("" -> "")
    newSubnet.get() foreach {
      case (name, addr) =>
        SubnetCache.addNew(name, addr)
    }
  }).
  add(4,4, button("Edit"){
    val idx = subnetList.getSelectedRow
    SubnetCache.get(idx) foreach { x =>
      newSubnet.setValue(x)
      newSubnet.get() foreach {
        case (name, addr) =>
          SubnetCache.set(idx, name, addr)
      }
    }
  }).
  add(6,4, button("Remove"){
    SubnetCache.remove(subnetList.getSelectedRow)
  }).
  add(8,4, button("OK"){
    returnNoValue()
  })
  
}

class NewSubnetDialog(parent:JFrame) extends ModalDialog[(String,String)](parent, "New subnetwork"){
  
  val nameField = new JTextField
  val addrField = new JTextField
  withLayout(this, "5dlu,0dlu:g,5dlu,0dlu:g,5dlu", "5dlu,p,5dlu,p,5dlu:g,p,5dlu").
  add(2,2, "Name:").
  add(2,4, "Address/mask:").
  add(4,2, nameField).
  add(4,4, addrField).
  add(2,6, button("Cancel"){
    returnNoValue()
  }).
  add(4,6, button("OK"){
    returnValue()
  })
  setSize(400,200)
  
  def readValue() = {
    (readName |@| readAddress)apply(_->_)
  }
  def displayValue(v:(String,String)) {
    nameField.setText(v._1)
    addrField.setText(v._2)
  }
  
  val VALID_NAME = "[a-z][a-z0-9]*"r
  
  def readName: Validation[NonEmptyList[String],String] = nameField.getText match {
    case t @ VALID_NAME() => t.success
    case _ => "Invalid subnetwork name".failNel
  }
  
  def readAddress: Validation[NonEmptyList[String],String] = {
    SubnetCache.validateRawSubnet(addrField.getText)
  }
  
}
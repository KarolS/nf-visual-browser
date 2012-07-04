/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfclient.swing.dialogs

import javax.swing._
import scalaz._
import Scalaz._
import pl.umk.mat.stasiu88.nfclient.SwingUtils._

class NewListItemDialog(
  parent: JFrame, 
  title:String,
  builders: ListItemBuilder*
) extends ModalDialog[ListItem](parent, title){
  
  require(builders.length > 0)
  
  val builderList = new JComboBox
  builderList.setEditable(false)
  builderList.setModel(new DefaultComboBoxModel(Array[AnyRef](builders:_*)))
  builderList.setSelectedIndex(0)
  builderList.withAction{
    refreshFieldEnableness()
  }
  val MAX_ROWS = 2
  val labels = Array.fill(MAX_ROWS)(new JLabel)
  val fields = Array.fill(MAX_ROWS)(new JTextField)
  refreshFieldEnableness()
  
  val lb = withLayout(
    this, 
    "5dlu,f:0dlu:g,5dlu,f:0dlu:g,5dlu",
    "5dlu,p,"+("5dlu,p,"*MAX_ROWS)+"5dlu:g,p,5dlu"
  ).
  add(2,2, 3,1, builderList).
  add(2,MAX_ROWS*2+4, button("Cancel"){
    returnNoValue()
  }).
  add(4,MAX_ROWS*2+4, button("OK"){
    returnValue()
  })
  
  for(i <- 0 until MAX_ROWS) {
    lb.add(2,i*2+4, labels(i))
    lb.add(4,i*2+4, fields(i))
  }
  setSize(400,200)
  
  def refreshFieldEnableness() {
    val builder = builderList.getSelectedItem.asInstanceOf[ListItemBuilder]
    val paramCount = builder.params.length
    for(i<-0 until MAX_ROWS){
      labels(i).setText(
        if(i<paramCount) builder.params(i).name
        else ""
      )
      fields(i).setEnabled(i<paramCount) //TODO: Field contents
    }
  }
  def readValue() = {
    val builder = builderList.getSelectedItem.asInstanceOf[ListItemBuilder]
    builder.build(fields.take(builder.params.length).map{_.getText})
  }
  def displayValue(value: ListItem) = {
    // not doing that
  }
  
}
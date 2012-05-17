package pl.umk.mat.stasiu88.nfclient.swing.dialogs

import javax.swing._
import scalaz._
import Scalaz._
import pl.umk.mat.stasiu88.nfclient.SwingUtils._


class CustomQueryDialog(parent:JFrame) extends ModalDialog[String](parent, "Custom query"){
  
  val textField = new JTextArea
  
  setSize(400,300)
  withLayout(this, "5dlu,f:p:g,5dlu,f:p:g,5dlu", "5dlu,f:p:g,5dlu,p,5dlu").
  add(2,2, 3,1, scrollable(textField)).
  add(2,4, button("Cancel"){
    returnNoValue()
  }).
  add(4,4, button("OK"){
    returnValue()
  })
  
  def readValue() = textField.getText.success
  def displayValue(a:String) = textField.setText(a)
}
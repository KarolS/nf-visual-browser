package pl.umk.mat.stasiu88.nfclient.swing.dialogs

import javax.swing._
import pl.umk.mat.stasiu88.nfclient.SwingUtils._
import scalaz._
import Scalaz._

class UsernamePasswordServerDialog(parent: JFrame) extends ModalDialog[(String,String,String)](parent, "") {
  val username = new JTextField
  val password = new JPasswordField
  val server   = new JTextField("http://localhost:8888")
  setSize(400,150)
  withLayout(
    this, 
    "5dlu,0dlu:g,5dlu,0dlu:g,5dlu", 
    "5dlu,p,5dlu,p,5dlu,p,5dlu:g,p,5dlu"
  ).
  add(2,2,"Username").
  add(2,4,"Password").
  add(2,6,"Server").
  add(4,2,username).
  add(4,4,password).
  add(4,6,server).
  add(2,8,button("Cancel"){
    returnNoValue()
  }).
  add(4,8,button("OK"){
    returnValue()
  })
  
  def readValue() = {
    (username.getText, new String(password.getPassword), server.getText).success //TODO: validation
  }
  def displayValue(s: (String,String,String)) = s match {
    case (u,p,s) => 
      username.setText(u)
      password.setText(p)
      server  .setText(s)
  }
}
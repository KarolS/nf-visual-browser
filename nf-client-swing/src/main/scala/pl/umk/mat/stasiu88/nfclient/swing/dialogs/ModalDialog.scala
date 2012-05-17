package pl.umk.mat.stasiu88.nfclient.swing.dialogs

import javax.swing.JDialog
import javax.swing.JFrame
import scalaz._
import Scalaz._
import pl.umk.mat.stasiu88.nfclient.SwingUtils._

abstract class ModalDialog[A](parent: JFrame, title: String) extends JDialog(parent,title,true) {
  
  
  var current_value:Option[A] = None
  
  protected def displayValue(a: A):Unit
  protected def readValue():Validation[NonEmptyList[String],A]
  
  def setValue(a:A) {
    displayValue(a)
  }
  def returnValue(){
    readValue() match {
      case Failure(errors) => 
        errorMessage(errors)
      case Success(a) =>
        current_value = Some(a)
        setVisible(false)
    }
  }
  def returnNoValue(){
    current_value = None
    setVisible(false)
  }
  def get():Option[A] = {
    current_value = None
    setVisible(true)
    current_value
  }
  
  def doIfChanged(f: A=>Unit): Boolean = {
    get() match {
      case Some(a) => 
        f(a)
        true
      case None => 
        false
    }
  }
}
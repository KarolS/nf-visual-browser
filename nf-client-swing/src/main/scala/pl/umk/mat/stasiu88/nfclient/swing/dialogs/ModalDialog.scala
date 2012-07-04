/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfclient.swing.dialogs

import javax.swing.JDialog
import javax.swing.JFrame
import scalaz._
import Scalaz._
import pl.umk.mat.stasiu88.nfclient.SwingUtils._

/**
 * Base class for simple dialog prompts asking a user for a single object.
 * <br>
 * Bazowa klasa dla prostych okien dialogowych pytających użytkownika o pojedynczy obiekt.
 */
abstract class ModalDialog[A](parent: JFrame, title: String) extends JDialog(parent,title,true) {
  
  
  var current_value:Option[A] = None
  
  /**
   * Sets the contents of components to given value.
   * <br>
   * Ustawia zawartość komponentów na daną wartość.
   */
  protected def displayValue(a: A):Unit
  /**
   * Tries to read the values from the components.
   * <br>
   * Próbuje sczytać wartości z komponentów.
   */
  protected def readValue():Validation[NonEmptyList[String],A]
  
  /**
   * Sets the contents of components to given value.
   * <br>
   * Ustawia zawartość komponentów na daną wartość.
   */
  def setValue(a:A) {
    displayValue(a)
  }
  /**
   * Tries to read values from components into the result and closes the window if succeeded.
   * <br>
   * Próbuje sczytać wartości z komponentów do wyniku i zamyka okno, jak się uda.
   */
  def returnValue(){
    readValue() match {
      case Failure(errors) => 
        errorMessage(errors)
      case Success(a) =>
        current_value = Some(a)
        setVisible(false)
    }
  }
  /**
   * Closes the window with no result.
   * <br>
   * Zamyka okno bez wyniku.
   */
  def returnNoValue(){
    current_value = None
    setVisible(false)
  }
  
  /**
   * Shows the window and returns a value if the user confirms.
   * <br>
   * Pokazuje okno i zwraca wartość, jeśli użytkownik potwierdzi.
   */
  def get():Option[A] = {
    current_value = None
    setVisible(true)
    current_value
  }
  /**
   * Shows the window and executes the block of code if user confirms.
   * <br>
   * Pokazuje okno i wykonuje block kodu, jeśli użytkownik potwierdzi. 
   */
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
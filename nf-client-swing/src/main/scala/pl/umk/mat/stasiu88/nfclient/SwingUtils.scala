/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfclient

import javax.swing._
import javax.swing.table._
import com.jgoodies.forms.layout.FormLayout
import com.jgoodies.forms.layout.CellConstraints
import java.awt.event._
import java.awt.Color
import scalaz.NonEmptyList
import java.awt.Graphics
import java.awt.event.MouseEvent
import java.awt.event.MouseEvent
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import java.io.File

/**
 * Utility functions for Swing.
 * <br>
 * Funkcje pomocnicze dla Swing.
 */
object SwingUtils {
  /**
   * Implicit conversions for Swing components.
   * <br>
   * Konwersje domyślne dla komponentów Swing.
   */
  implicit def augmentComponent(source: JComponent) = new {
    /**
     * Adds an action handler.
     * <br>
     * Dodaje obsługę akcji.
     */
    def withAction(action: ActionEvent=>Any) = {
      source.asInstanceOf[{
        def addActionListener(a: ActionListener):Unit
      }].addActionListener(new ActionListener{
        def actionPerformed(ev: ActionEvent){
          action(ev)
          ()
        }
      })
      source
    }
    /**
     * Adds an action handler.
     * <br>
     * Dodaje obsługę akcji.
     */
    def withAction(action: =>Any) = {
      source.asInstanceOf[{
        def addActionListener(a: ActionListener):Unit
      }].addActionListener(new ActionListener{
        def actionPerformed(ev: ActionEvent){
          action
          ()
        }
      })
      source
    }
    /**
     * Adds a resize handler.
     * <br>
     * Dodaje reakcję na zmianę rozmiaru.
     */
    def withResize(action: => Unit) = {
      source.addComponentListener(new ComponentListener(){
        def componentHidden(ev: ComponentEvent){}
        def componentMoved(ev: ComponentEvent){}
        def componentShown(ev: ComponentEvent){}
        def componentResized(ev: ComponentEvent){
          action
        }
      })
      source
    }
    
    def withDynamicTooltip(tooltip: (Int,Int)=>Option[String]) = {
      source.addMouseMotionListener(new MouseMotionListener{
        def mouseDragged(ev: MouseEvent){}
        def mouseMoved(ev: MouseEvent){
          source.setToolTipText(tooltip(ev.getX,ev.getY).orNull)
        }
      })
      source
    }
    
    def withMouseWheelSupport(callback: Int=>Unit) = {
      source.addMouseWheelListener(new MouseWheelListener{
        def mouseWheelMoved(ev: MouseWheelEvent){
          callback(ev.getWheelRotation)
        }
      })
      source
    }
  }
  
  /**
   * Creates a button with given caption and action handler.
   * <br>
   * Tworzy przycisk z daną etykietą i obsługą akcji.
   */
  def button(text: String)(action: =>Unit) = {
    val source = new JButton(text)
    source.addActionListener(new ActionListener{
      def actionPerformed(ev: ActionEvent){
        action
      }
    })
    source
  }
  /**
   * Creates a table with scrollbars from a given model.
   * <br>
   * Tworzy tabelę z paskami przewijania z danego modelu.
   */
  def scrollableTable(model: TableModel) = scrollable(new JTable(model))
  
  /**
   * Wraps a component and adds scrollbars.
   * <br>
   * Zawija komponent i dodaje mu paski przewijania.
   */
  def scrollable(component: JComponent) = {
    val sp = new JScrollPane(component)
    sp.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS)
    sp.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED)
    component match {
      case table:JTable => table.setFillsViewportHeight(true)
      case _ => ()
    }
    sp
  }
  /**
   * Creates a JPanel with given painting function.
   * <br>
   * Tworzy JPanel z podaną funkcją rysującą.
   */
  def canvas(f: (Graphics,Int,Int)=>Unit) = {
    new JPanel{
      override def paint(g:Graphics){
        val w = getWidth
        val h = getHeight
        g.setColor(Color.WHITE)
        g.fillRect(0,0,w,h)
        f(g, w, h)
      }
    }
  }
  /**
   * Exports a PNG image file with given dimensions and painting function.
   * <br>
   * Eksportuje plik obrazu PNG z danymi wymiarami i funkcją rysującą.
   */
  def exportPng(filename: String, w:Int, h:Int)(f: (Graphics,Int,Int)=>Unit){
    val bi = new BufferedImage(w, h,BufferedImage.TYPE_INT_ARGB)
    f(bi.createGraphics, w, h)
    val outputfile = new File(filename)
    ImageIO.write(bi, "png", outputfile)
  }

  implicit def actionListener(action: ActionEvent=>Unit) = new ActionListener{
    def actionPerformed(ev: ActionEvent){
      action(ev)
    }
  }
  implicit def actionListener(action: =>Unit) = new ActionListener{
    def actionPerformed(ev: ActionEvent){
      action
    }
  }
  
  /**
   * Creates an empty JPanel of given colour.
   * <br>
   * Tworzy pusty JPanel danego koloru.
   */
  def colour(colour: Color) = {
    val p = new JPanel
    p.setBackground(colour)
    p
  }
  
  /**
   * Creates a checkbox that is used only to enable and disable a certain component.
   * <br>
   * Tworzy pole wyboru, którego jedynym zadaniem jest włączanie i wyłączanie danego komponentu. 
   */
  def enabler(text: String, default: Boolean, target: JComponent) = {
    val ch = new JCheckBox(text)
    ch.setSelected(default)
    target.setEnabled(default)
    ch.addActionListener{
      target.setEnabled(ch.isSelected)
    }
    ch
  }
  implicit def toLabel(text: String):JComponent = new JLabel(text)
  
  /**
   * Creates a <code>LayoutBuilder</code>.
   * <br>
   * Tworzy <code>LayoutBuilder</code>.
   */
  def withLayout(component: java.awt.Container, cols:String, rows:String) = {
    new LayoutBuilder(component, cols, rows)
  }
  def withOneComponent(parent: java.awt.Container, kid: JComponent) = {
    withLayout(parent,"f:p:g","f:p:g").add(1,1,kid)
  }
  
  /**
   * Invokes given block of code in AWT event dispatch thread later.
   * <br>
   * Uruchamia później dany blok kodu w wątku obsługi zdarzeń AWT.
   */
  def invokeLater(f: =>Unit){
    SwingUtilities.invokeLater(new Runnable{
      def run = { f }
    })
  }
  /**
   * Invokes given block of code in AWT event dispatch thread now.
   * <br>
   * Uruchamia teraz dany blok kodu w wątku obsługi zdarzeń AWT.
   */
  def invokeAndWait(f: =>Unit){
    if(SwingUtilities.isEventDispatchThread){
      f
    } else {
      SwingUtilities.invokeAndWait(new Runnable{
        def run = { f }
      })
    }
  }
  
  /**
   * Displays an information message.
   * <br>
   * Wyświetla komunikat informacyjny.
   */
  def infoMessage(s:String) = invokeAndWait {
    JOptionPane.showMessageDialog(null, s, "Information", JOptionPane.INFORMATION_MESSAGE)
  }
  
  /**
   * Displays an error message.
   * <br>
   * Wyświetla komunikat z błędem.
   */
  def errorMessage(s:String) = invokeAndWait {
    JOptionPane.showMessageDialog(null, s, "Error", JOptionPane.ERROR_MESSAGE)
  }
  
  implicit def toNormalList[A](nel: NonEmptyList[A]): List[A] = nel.head::nel.tail
  
  /**
   * Displays an error message.
   * <br>
   * Wyświetla komunikat z błędem.
   */
  def errorMessage(s: NonEmptyList[String]) {
    errorMessage(s.mkString("\n"))
  }
  /**
   * Displays an error message.
   * <br>
   * Wyświetla komunikat z błędem.
   */
  def errorMessage(header:String, s: NonEmptyList[String]) {
    errorMessage(header + "\n" + s.mkString("\n"))
  }
  /**
   * Displays a confirmation dialog and executes given block of code if uses chooses Yes.
   * <br>
   * Wyświetla okno dialogowe z pytaniem i wykonuje dany blok kodu, jeśli użytkownik wybierze Tak.
   */
  def confirm(question:String)(f: =>Unit) {
    val answer = JOptionPane.showConfirmDialog(
      null, 
      question, 
      "Confirmation",
      JOptionPane.YES_NO_OPTION
    )
    if(answer == JOptionPane.YES_OPTION){
      f
    }
  }
  /**
   * Creates new <code>MainMenuBuilder</code> and attaches the menu to the frame.
   * <br>
   * Tworzy nowy <code>MainMenuBuilder</code> i dołącza menu do okna.
   */
  def withMenu(frame: JFrame) = {
    val menu = new JMenuBar
    frame.setJMenuBar(menu)
    new MainMenuBuilder(menu)
  }
  
  private val fileChooser = new JFileChooser
  fileChooser.setDialogTitle("Save HTML file")
  
  /**
   * Asks user for a file name to save.
   * <br>
   * Pyta użytkownika o nazwę pliku do zapisu. 
   */
  def chooseFile(parent: JComponent) = {
    if(fileChooser.showSaveDialog(parent) == JFileChooser.APPROVE_OPTION) {
      Some(fileChooser.getSelectedFile)
    }
    else{
      None
    }
  }
}
/**
 * Helper class used to construct menus.
 * <br>
 * Klasa pomocnicza służąca do budowy menu.
 */
class MenuBuilder(thisMenu: JMenu){
  def item(title: String)(action: =>Unit) = {
    import SwingUtils._
    val item = new JMenuItem(title)
    item.addActionListener{action}
    thisMenu.add(item)
    this
  }
  def separator = {
    thisMenu.addSeparator()
    this
  }
  
}
/**
 * Helper class used to construct main menus.
 * <br>
 * Klasa pomocnicza służąca do budowy menu głównych.
 */
class MainMenuBuilder(bar: JMenuBar) {
  def menu(title: String)(m: MenuBuilder=>Unit) = {
    val menu = new JMenu(title)
    m(new MenuBuilder(menu))
    bar.add(menu)
    this
  }
}

/**
 * Helper class used to construct layouts.
 * <br>
 * Klasa pomocnicza służąca do budowy layoutów.
 */
class LayoutBuilder(component: java.awt.Container, cols:String, rows:String){
  component.setLayout(new FormLayout(cols,rows))
  val cc = new CellConstraints
  /**
   * Adds a component to given cell.
   * <br>
   * Dodaje komponent w danej komórce.
   */
  def add(x:Int, y:Int, kid: JComponent):LayoutBuilder = {
    add(x,y,1,1,kid)
  }
  /**
   * Adds a component of given dimensions to given cell.
   * <br>
   * Dodaje komponent danych rozmiarów w danej komórce.
   */
  def add(x:Int, y:Int, w:Int, h:Int, kid: JComponent) = {
    component.add(kid, cc.xywh(x,y,w,h))
    this
  }
  /**
   * Adds a panel with given layout.
   * <br>
   * Dodaje panel z danym layoutem.
   */
  def add(x:Int, y:Int, cols:String, rows:String)(kid: LayoutBuilder=>Unit):LayoutBuilder = {
    add(x,y,1,1,cols,rows)(kid)
  }
  /**
   * Adds a panel with given layout.
   * <br>
   * Dodaje panel z danym layoutem.
   */
  def add(x:Int, y:Int, w:Int, h:Int, cols:String, rows:String)(kid: LayoutBuilder=>Unit) = {
    val panel = new JPanel
    component.add(panel, cc.xywh(x,y,1,1))
    panel.setLayout(new FormLayout(cols,rows))
    kid(new LayoutBuilder(panel, cols, rows))
    this
  }
}
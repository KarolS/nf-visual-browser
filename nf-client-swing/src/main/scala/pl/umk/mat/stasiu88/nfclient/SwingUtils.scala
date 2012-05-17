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

object SwingUtils {
  
  implicit def augmentComponent(source: JComponent) = new {
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
  
  def button(text: String)(action: =>Unit) = {
    val source = new JButton(text)
    source.addActionListener(new ActionListener{
      def actionPerformed(ev: ActionEvent){
        action
      }
    })
    source
  }
  def scrollableTable(model: TableModel) = scrollable(new JTable(model))
  
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
  
  def colour(colour: Color) = {
    val p = new JPanel
    p.setBackground(colour)
    p
  }
  
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
  
  def withLayout(component: java.awt.Container, cols:String, rows:String) = {
    new LayoutBuilder(component, cols, rows)
  }
  def withOneComponent(parent: java.awt.Container, kid: JComponent) = {
    withLayout(parent,"f:p:g","f:p:g").add(1,1,kid)
  }
  
  def invokeLater(f: =>Unit){
    SwingUtilities.invokeLater(new Runnable{
      def run = { f }
    })
  }
  def invokeAndWait(f: =>Unit){
    if(SwingUtilities.isEventDispatchThread){
      f
    } else {
      SwingUtilities.invokeAndWait(new Runnable{
        def run = { f }
      })
    }
  }
  
  def errorMessage(s:String) = invokeAndWait {
    JOptionPane.showMessageDialog(null, s, "Error", JOptionPane.ERROR_MESSAGE)
  }
  
  implicit def toNormalList[A](nel: NonEmptyList[A]): List[A] = nel.head::nel.tail
  
  def errorMessage(s: NonEmptyList[String]) {
    errorMessage(s.mkString("\n"))
  }
  def errorMessage(header:String, s: NonEmptyList[String]) {
    errorMessage(header + "\n" + s.mkString("\n"))
  }
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
  def withMenu(frame: JFrame) = {
    val menu = new JMenuBar
    frame.setJMenuBar(menu)
    new MainMenuBuilder(menu)
  }
  
  private val fileChooser = new JFileChooser
  fileChooser.setDialogTitle("Save HTML file")
  
  def chooseFile(parent: JComponent) = {
    if(fileChooser.showSaveDialog(parent) == JFileChooser.APPROVE_OPTION) {
      Some(fileChooser.getSelectedFile)
    }
    else{
      None
    }
  }
}
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
class MainMenuBuilder(bar: JMenuBar) {
  def menu(title: String)(m: MenuBuilder=>Unit) = {
    val menu = new JMenu(title)
    m(new MenuBuilder(menu))
    bar.add(menu)
    this
  }
}

class LayoutBuilder(component: java.awt.Container, cols:String, rows:String){
  component.setLayout(new FormLayout(cols,rows))
  val cc = new CellConstraints
  def add(x:Int, y:Int, kid: JComponent):LayoutBuilder = {
    add(x,y,1,1,kid)
  }
  def add(x:Int, y:Int, w:Int, h:Int, kid: JComponent) = {
    component.add(kid, cc.xywh(x,y,w,h))
    this
  }
  def add(x:Int, y:Int, cols:String, rows:String)(kid: LayoutBuilder=>Unit):LayoutBuilder = {
    add(x,y,1,1,cols,rows)(kid)
  }
  def add(x:Int, y:Int, w:Int, h:Int, cols:String, rows:String)(kid: LayoutBuilder=>Unit) = {
    val panel = new JPanel
    component.add(panel, cc.xywh(x,y,1,1))
    panel.setLayout(new FormLayout(cols,rows))
    kid(new LayoutBuilder(panel, cols, rows))
    this
  }
}
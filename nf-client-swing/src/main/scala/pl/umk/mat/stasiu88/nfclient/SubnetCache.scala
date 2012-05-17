package pl.umk.mat.stasiu88.nfclient

import scala.collection.mutable.ArrayBuffer
import javax.swing.table._
import scalaz._
import Scalaz._

object SubnetCache extends AbstractTableModel{
  
  private[this] val cache = new ArrayBuffer[(String,String)]()
  
  def hasName(n: String) = synchronized {
    cache.exists(_._1 == n)
  }
  def code = cache.map{t => t._1 + " " + t._2}.mkString(" ") 
  
 
  def getRowCount = synchronized{
    cache.length
  }
  
  def getColumnCount = 2
  
  override def getColumnName(i:Int) = i match {
    case 0 => "Name"
    case 1 => "Address space"
  } 
  def getValueAt(j:Int, i:Int) = synchronized {
    try{
      i match {
        case 0 => cache(j)._1
        case 1 => cache(j)._2
      }
    } catch {
      case _ => 
        "Index out of bounds: "+(i,j)
    }
  }
  
  override def isCellEditable(x:Int, y: Int) = false
  override def setValueAt(o: AnyRef, x: Int, y: Int) = ()

  def addNew(name: String, address: String) = {
    synchronized {
      cache += name -> address
    }
    fireTableDataChanged()
  }
  def set(idx: Int, name: String, address: String) = {
    synchronized {
      if(idx>=0 && idx<cache.length) cache(idx) = name -> address
    }
    fireTableDataChanged()
  }
  
  def remove(idx:Int) = {
    synchronized {
      if(idx>=0 && idx<cache.length) cache.remove(idx)
    }
    fireTableDataChanged()
  }
  
  def get(idx: Int) ={
    synchronized{
      if(idx>=0 && idx<cache.length) Some(cache(idx))
      else None
    }
  }
  
  val SEMI_VALID_IP4_A = """[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+/[0-9]+"""r
  val SEMI_VALID_IP4_B = """[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+/[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+"""r
  val SEMI_VALID_IP4_C = """[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+"""r
  //TODO: more, e.g. IPv6
  
  
  def validateSubnet(t:String) = synchronized {
    if(cache.exists(_._1==t)) t.success
    else validateRawSubnet(t)
  }
  def validateRawSubnet(t:String) = {
    val err = "Invalid address or mask".failNel
    def isByte(i:Int) = {
      i>=0 && i<=255
    }
    def isMaskByte(i: Int) = {
      val neg = (~i)&0xff
      isByte(i)&&(neg&(neg+1))==0
    }
    t match {
      case t @ SEMI_VALID_IP4_A() =>
        try{
          val a = t.split("/")
          val addr = a(0).split(".").map{_.toInt}.toList
          val bits = a(1).toInt
          if(addr.all(isByte _) && bits>=0 && bits<=32){
            t.success
          } else err
        } catch {
          case _ => err
        }
      case t @ SEMI_VALID_IP4_B() =>
        try{
          val a = t.split("/")
          val addr = a(0).split(".").map{_.toInt}.toList
          val bits = a(1).split(".").map{_.toInt}.toList
          if(addr.all(isByte _) && bits.all(isMaskByte _) && bits.sortBy{-_} == bits){
            t.success
          } else err
        } catch {
          case _ => err
        }
      case t @ SEMI_VALID_IP4_C() =>
        try{
          val addr = t.split(".").map{_.toInt}.toList
          if(addr.all(isByte _)){
            t.success
          } else err
        } catch {
          case _ => err
        }
      case _ => err
    }
   
  }
}
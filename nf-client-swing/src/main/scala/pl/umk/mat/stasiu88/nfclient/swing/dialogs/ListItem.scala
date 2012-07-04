/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfclient.swing.dialogs

import scalaz._
import Scalaz._
import java.util.Locale
import pl.umk.mat.stasiu88.nfclient.SubnetCache

/**
 * An object representing a chunk of code hidden behind human-readable name.
 * <br>
 * Obiekt reprezentujący kawałek kodu ukryty za czytelną dla człowieka nazwą. 
 */
case class ListItem(displayForm: String, code: String){
  override def toString = displayForm
}
object ListItemBuilder{
  /**
   * Creates a ListItemBuilder that simply intersperses given parts with parameter values.
   * <br>
   * Tworzy ListItemBuilder, który po prostu przeplata dane fragmenty wartościami parametrów. 
   */
  def apply(displayForm: String, params: Seq[Param], parts: String*): ListItemBuilder ={
    require(parts.length == params.length+1)
    ListItemBuilder(displayForm, params, { seq =>
      parts.head + (seq zip parts.tail).map{t => t._1+t._2}.mkString("")
    })
  }
}
/**
 * A parameterized ListItem factory.
 * <br>
 * Parametryzowalna faktoria obiektów ListItem.
 */
case class ListItemBuilder(displayForm: String, params: Seq[Param], builder: Seq[String]=>String){
  /**
   * Tries to create a ListItem from a list of parameter values.
   * <br>
   * Próbuje stworzyć ListItem z listy wartości paramterów.
   */
  def build(paramValues: Seq[String]): Validation[NonEmptyList[String],ListItem] = {
    val validated = (params zip paramValues).map {
      case (param, value) => param parse value
    }.sequence[({type l[a]=Validation[NonEmptyList[String], a]})#l, String] 
    val code = validated.map{builder}
    val itemDisplayForm = displayForm +" "+ paramValues.mkString(" & ")
    (itemDisplayForm.success |@| code) apply (ListItem(_,_))
  }
  
  override def toString = displayForm
}
/**
 * Represents and validates a parameter for a ListItem.
 * <br>
 * Reprezentuje i waliduje parametr dla ListItem.
 */
abstract class Param(val name: String){
  def parse(input: String): Validation[NonEmptyList[String],String]
}

/**
 * Validators for strings.
 * <br>
 * Walidatory dla łańcuchów.
 */
object ParamUtils {
  def validateInterface(errorMsg: String, input: String) = {
    try{
      val i = input.trim.toInt
      if(i<0 || i>65535) errorMsg.failNel //TODO
      else i.toString.success
    } catch{
      case _ => errorMsg.failNel
    } 
  }
  def validatePort(errorMsg: String, input: String) = {
    try{
      val i = input.trim.toInt
      if(i<=0 || i>65535) errorMsg.failNel
      else i.toString.success
    } catch{
      case _ => errorMsg.failNel
    } 
  }
  def validateByte(errorMsg: String, input: String) = {
    try{
      val i = input.trim.toInt
      if(i<0 || i>255) errorMsg.failNel
      else i.toString.success
    } catch{
      case _ => errorMsg.failNel
    } 
  }
  def validateProtocol(errorMsg: String, input: String) = {
    try{
      val i = input.trim.toInt
      if(i<0 || i>255) errorMsg.failNel
      else ("proto "+i).success
    } catch{
      case _ => 
        input.trim.toLowerCase(Locale.US) match {
          case "tcp" => "tcp".success
          case "udp" => "udp".success
          case "icmp" => "icmp".success
          //TODO: more protocols
          case _ => errorMsg.failNel
        }
    } 
  }
  
  def validateSubnet(errorMsg: String, input: String) = {
    SubnetCache.validateSubnet(input)
  }
  def validateIP(errorMsg: String, input: String) = {
    input.success //TODO
  }
}

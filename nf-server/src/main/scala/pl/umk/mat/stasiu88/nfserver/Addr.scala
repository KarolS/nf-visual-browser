/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver

/**
 * Abstract class representing IP address.
 * <br>
 * Klasa abstrakcyjna reprezentująca adres IP.
 */
abstract class Addr {
  /**
   * Tests if the address belongs to the subnet.
   * <br>
   * Testuje, czy adres należy do podsieci.
   */
  def ~(subnet: Subnet): Boolean = subnet(this)
  /**
   * Converts the address to the list of 32-bit integer.
   * <br>
   * Konwertuje adres do postaci listy 32-bitowych liczb całkowitych.
   */
  def toIntList: List[Int]
}
object Addr {
  val IPV4REGEX = """\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}"""r
  val IPV6REGEX = """[a-fA-F0-9]{0,4}(:[a-fA-F0-9]{0,4})+"""r
  /**
   * Converts string to IP address.
   * <br>
   * Konwertuje łańcuch do adresu IP. 
   */
  def apply(string: String): Addr = string match {
    case IPV4REGEX() => IP4Addr(string)
    case IPV6REGEX() => IP6Addr(string)
  }
}

/**
 * IPv4 address.
 * <br>
 * Adres IPv4.
 */
case class IP4Addr(value: Int) extends Addr {
  override def toString = {
    "%d.%d.%d.%d" format ((value >>> 24) & 255, (value >>> 16) & 255,
      (value >>> 8) & 255, (value & 255))
  }
  def toIntList = List(4, value)
}
/**
 * IPv6 address.
 * <br>
 * Adres IPv6.
 */
case class IP6Addr(part0: Long, part1: Long) extends Addr {
  //TODO: override def toString = ...
  def toIntList = List(6, (part0>>>32).toInt, (part0).toInt, (part1>>>32).toInt, (part1).toInt)
}
object IP4Addr {
  val ZERO = IP4Addr("0.0.0.0") 
  val LOCALHOST = IP4Addr("127.0.0.1") 
  /**
   * Converts string with IPv4 address in form of dot-separated decimal bytes to IP address.
   * <br>
   * Konwertuje łańcuch z adresem IPv4 w postaci oddzielonych kropkami bajtów zapisanych dziesiętnie do adresu IP. 
   */
  def apply(string: String): IP4Addr = {
    val parts = string.split("""\.""")
    if (parts.length != 4) throw new NumberFormatException
    var intparts = parts map { _.toInt }
    if (intparts exists { x => x < 0 || x > 255 }) throw new NumberFormatException
    new IP4Addr(intparts.foldLeft(0){ _ * 256 + _ })
  }
}
object IP6Addr {
  val ZERO = new IP6Addr(0L, 0L) 
  val LOCALHOST = new IP6Addr(0L, 1L) 
  /**
   * Converts a list of integers to an IPv6 address.
   * <br>
   * Konwertuje listę liczb to adresu IPv6.
   */
  def apply(ip0:Int, ip1:Int, ip2: Int, ip3:Int)={
    new IP6Addr(
        ip0.toLong<<32 + ip1,
        ip2.toLong<<32 + ip3
    ) 
  }
  /**
   * Converts string with IPv6 address in form of colon-separated hexadecimal words to IP address.
   * <br>
   * Konwertuje łańcuch z adresem IPv6 w postaci oddzielonych dwukropkami słow zapisanych szestnastkowo do adresu IP. 
   */
  def apply(string: String) = {
    val words = Array.fill(8)(0)
    var pointer = 0
    var madejump = false
    val blocks = string.split(":")
    for(block<-blocks) block match {
      case "" => 
        if(madejump) pointer+=1 else pointer += 9-blocks.length
        madejump = true
      case _  => 
        words(pointer) = Integer.parseInt(block,16)
        pointer+=1
    }
    new IP6Addr(
      words.take(4).foldLeft(0L){ _ * 256L + _ },
      words.drop(4).foldLeft(0L){ _ * 256L + _ }
    )
  }
}
/**
 * Abstract class representing IP address range.
 * <br>
 * Klasa abstrakcyjna reprezentująca zakres adresów IP.
 */
abstract class Subnet extends Function1[Addr, Boolean] {
  def toIntList: List[Int]
}

object Subnet {
  /**
   * Creates IP address range based on base IP address and address mask.
   * <br>
   * Tworzy zakres adresów IP oparty o bazowy adres i maskę. 
   */
  def apply(subnet: Addr, mask: Addr): Subnet = (subnet, mask) match {
    case (IP4Addr(v), IP4Addr(w)) => IP4Subnet(v, w)
    case (IP6Addr(v0,v1), IP6Addr(w0,w1)) => IP6Subnet(v0,v1, w0,w1)
  }
  /**
   * Creates IP address range based on base IP address and address mask length.
   * <br>
   * Tworzy zakres adresów IP oparty o bazowy adres i ilość bitów maski. 
   */
  def apply(subnet: Addr, maskbits: Int): Subnet = subnet match {
    case IP4Addr(v) if maskbits >= 0 && maskbits <= 32 => IP4Subnet(v, ~((1 << (32 - maskbits)) - 1))
    case i:IP6Addr if maskbits >= 0 && maskbits <= 128 => IP6Subnet(i, maskbits)
  }
  /**
   * Creates IP address range with a single IP
   * <br>
   * Tworzy zakres adresów IP oparty o bazowy adres i maskę. 
   */
  def apply(subnet: Addr): Subnet = subnet match {
    case IP4Addr(v) => IP4Subnet(v, -1)
    case IP6Addr(v0,v1) => IP6Subnet(v0,v1, -1L, -1L)
  }
}
/**
 * IPv4 address range.
 * <br>
 * Zakres adresów IPv4.
 */
case class IP4Subnet(subnet: Int, mask: Int) extends Subnet {
  override def toString = IP4Addr(subnet) + "/" + IP4Addr(mask)
  def apply(addr: Addr) = addr match {
    case IP4Addr(value) => (value & mask) == (subnet & mask)
    case _              => false
  }
  def toIntList: List[Int] = List(4, subnet, mask)
}
/**
 * IPv6 address range.
 * <br>
 * Zakres adresów IPv6.
 */
case class IP6Subnet(subnet0: Long, subnet1: Long, mask0: Long, mask1: Long) extends Subnet {
  override def toString = IP6Addr(subnet0,subnet1) + "/" + IP6Addr(mask0,mask1)
  def apply(addr: Addr) = addr match {
    case IP6Addr(value0, value1) => 
      ((value0 & mask0) == (subnet0 & mask0)) && ((value1 & mask1) == (subnet1 & mask1))
    case _              => false
  }
  def toIntList: List[Int] = List( 6, 
    (subnet0>>>32).toInt, (subnet0).toInt,
    (subnet1>>>32).toInt, (subnet1).toInt,
    (mask0>>>32).toInt, (mask0).toInt,
    (mask1>>>32).toInt, (mask1).toInt        )
}

object IP4Subnet {
  /**
   * Creates IP address range based on base IP address and address mask length.
   * <br>
   * Tworzy zakres adresów IP opary o bazowy adres i ilość bitów maski. 
   */
  def apply(subnet: String, maskBits: Int) {
    val ip = IP4Addr(subnet).value
    val mask = ~((1 << (32 - maskBits)) - 1)
    new IP4Subnet(ip, mask)
  }
}
object IP6Subnet {
  /**
   * Creates IP address range based on base IP address and address mask length.
   * <br>
   * Tworzy zakres adresów IP oparty o bazowy adres i ilość bitów maski. 
   */
  def apply(ip: IP6Addr, maskBits: Int) = {
    val mask0 = if(maskBits>=64) -1 else ~((1 << ( 64 - maskBits)) - 1)
    val mask1 = if(maskBits< 64)  0 else ~((1 << (128 - maskBits)) - 1)
    new IP6Subnet(ip.part0, ip.part1, mask0, mask1)
  }
  /**
   * Creates IP address range based on base IP address and address mask length.
   * <br>
   * Tworzy zakres adresów IP opary o bazowy adres i ilość bitów maski. 
   */
  def apply(subnet: String, maskBits: Int) {
    val ip = IP6Addr(subnet)
    IP6Subnet(ip, maskBits)
  }
}

/**
 * Placeholder for actual IP ranges in partially parsed queries.
 * <br>
 * Zastępuje rzeczywisty zakres IP w częściowo sparsowanych zapytaniach.
 */
case class NamedSubnet(name:String) extends Subnet {
  def apply(addr: Addr) = throw new IllegalStateException("Executing non-cleaned query")
  def toIntList: List[Int] = throw new IllegalStateException("Executing non-cleaned query")
}

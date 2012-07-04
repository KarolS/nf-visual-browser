/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver

import scala.collection.mutable
import java.net.InetAddress

/**
 * Not-yet working implementation of DNS cache.
 * <br>
 * Jeszcze niedziałająca implementacja cache DNS.
 */
object DnsCache {

  private[this] val cache = mutable.Map[Addr,Set[Symbol]]().withDefault(getForTheFirstTime(_))

  private[this] val dnsCache = mutable.Map[Symbol, Set[Addr]]().withDefault(dnsGetForTheFirstTime(_))
  
  private def dnsGetForTheFirstTime(hostname: Symbol):Set[Addr] = synchronized{
    val r = try{
      Set(Addr(InetAddress.getByName(hostname.name).getHostAddress))
    } catch {
      case _ => Set[Addr]()
    }
    r foreach{ x =>
      cache(x) = cache(x) + hostname
    }
    r
  }
  private def getForTheFirstTime(addr: Addr): Set[Symbol] = synchronized{
    val v1 = try{
      Set(Symbol(
        InetAddress.getByName(addr.toString).getCanonicalHostName
      ))
    } catch {
      case _ => Set()
    }
    val v2 = try{
      Set(Symbol(
        InetAddress.getByName(addr.toString).getHostName
      ))
    } catch {
      case _ => Set()
    }
    v1 ++ v2
  }
  private def getForTheFirstTime(s: Symbol): Option[Addr] = synchronized{
    try{
      Some(Addr(InetAddress.getByName(s.name).getHostAddress))
    } catch {
      case _ => None
    }
  }
  def equal(addr: Addr, hostname: Symbol)={
    cache(addr).contains(hostname)
  }
  def canonicalHostname(addr: Addr) = {
    cache(addr).headOption.getOrElse(Symbol(addr.toString))
  }
  
  def inDomain(addr: Addr, domain: Symbol)={
    if(domain.name.startsWith(".")){
      cache(addr).exists(_.name.endsWith(domain.name))
    } else {
      cache(addr).exists(x => x==domain || x.name.endsWith("."+domain.name))
    }
  }

}
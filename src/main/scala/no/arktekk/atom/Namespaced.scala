package no.arktekk.atom

import com.codecommit.antixml.QName

/**
 * Created by IntelliJ IDEA.
 * User: maedhros
 * Date: 1/2/12
 * Time: 12:15 PM
 * To change this template use File | Settings | File Templates.
 */

case class Namespaced(namespace: String, prefix: Option[String], name: String) {
  require(namespace != null, "Null namespace not allowed")
  require(name != null, "Null name not allowed")

  def toMap = {
    Map((prefix.getOrElse("") -> namespace))
  }
}

object Namespaced {
  def apply(namespace:String, prefix: String, name:String): Namespaced = apply(namespace, Some(prefix), name)
  def apply(namespace:String, qname: QName): Namespaced = apply(namespace, qname.prefix, qname.name)
  def apply(namespace:String, name: String): Namespaced = apply(namespace, None, name)
}

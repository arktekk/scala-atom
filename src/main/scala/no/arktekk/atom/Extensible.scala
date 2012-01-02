package no.arktekk.atom

import com.codecommit.antixml.Elem


/**
 * Created by IntelliJ IDEA.
 * User: maedhros
 * Date: 1/2/12
 * Time: 12:16 PM
 * To change this template use File | Settings | File Templates.
 */

trait Extensible {
  type A <: Extensible

  def wrapped: Elem

  def getExtension[A <: ElementWrapper](ns: Namespaced)(implicit manifest: Manifest[A]): Option[A] = {
    getExtensions(ns).headOption
  }

  def getExtensions[A <: ElementWrapper](ns: Namespaced)(implicit manifest: Manifest[A]): List[A] = {
    val selected = (wrapped \ namespaceSelector(ns.namespace, ns.name))
    val ctor = Option(manifest.erasure.getConstructor(classOf[Elem]))
    ctor.foreach(_.setAccessible(true))
    selected.flatMap {
      case s: Elem => ctor.map(_.newInstance(s).asInstanceOf[A])
    }.toList
  }

  def copy(wrapped: Elem): A

  def addExtension(wrapper: ElementWrapper): A = {
    copy(wrapped.copy(children = wrapped.children :+ wrapper.wrapped))
  }
}

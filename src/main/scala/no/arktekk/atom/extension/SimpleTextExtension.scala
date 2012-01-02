package no.arktekk.atom.extension

import no.arktekk.atom.{Namespaced, ElementWrapper}
import com.codecommit.antixml._

/**
 * Created by IntelliJ IDEA.
 * User: maedhros
 * Date: 1/2/12
 * Time: 1:22 PM
 * To change this template use File | Settings | File Templates.
 */

class SimpleTextExtension(elem: Elem) extends ElementWrapper(elem) {
  def value = (wrapped \ text).head
  
  def addAttribute(name: QName, value: String) = new SimpleTextExtension(elem.copy(attrs = elem.attrs + (name -> value)))

  def removeAttribute(name: QName) = new SimpleTextExtension(elem.copy(attrs = elem.attrs - name))
  
  def withValue(text: String) = new SimpleTextExtension(elem.copy(children = Group(Text(text))))
}

object SimpleTextExtension {
  def apply(namespaced: Namespaced, value: String): SimpleTextExtension = {
    new SimpleTextExtension(Elem(namespaced.prefix, namespaced.name, Attributes(), namespaced.toMap, Group(Text(value))))
  }
  
  def unapply(e: SimpleTextExtension) = Some(e.wrapped)
}

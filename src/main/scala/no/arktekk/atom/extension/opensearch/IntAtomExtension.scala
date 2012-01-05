package no.arktekk.atom.extension.opensearch

import no.arktekk.atom.Atom._
import no.arktekk.atom.extension.{OptionSelectableElementWrapperAtomExtension, SimpleTextElementWrapper}
import no.arktekk.atom.{FeedLike, Namespaced, ElementWrapper}
import com.codecommit.antixml.text

/**
 * Created by IntelliJ IDEA.
 * User: maedhros
 * Date: 1/5/12
 * Time: 12:42 PM
 * To change this template use File | Settings | File Templates.
 */

class IntAtomExtension(name:String) extends OptionSelectableElementWrapperAtomExtension[FeedLike, Int] {
  val selector = namespaceSelector(openSearchNamespace, name)

  def function = (e) => (e \ text).headOption.map(_.toInt)

  def toElem(a: Option[Int], wrapper: ElementWrapper) = {
    a.map(x => SimpleTextElementWrapper(Namespaced(openSearchNamespace, defaultPrefix, name), x.toString)).toSeq
  }
}

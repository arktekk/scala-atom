package no.arktekk.atom.extension

import com.codecommit.antixml.{Elem, Selector}
import no.arktekk.atom.{ElementWrapper}

/**
 * Created by IntelliJ IDEA.
 * User: maedhros
 * Date: 1/5/12
 * Time: 12:08 PM
 * To change this template use File | Settings | File Templates.
 */

trait SelectableElementWrapperAtomExtension[A <: ElementWrapper, B] extends AtomExtension[A, B] {
  def selector: Selector[Elem]
  def function: (Elem) => B

  protected def select(elem: Elem, selector: Selector[Elem], f: (Elem) => B): B

  def fromLike(like: A) = select(like.wrapped, selector, function)
}

trait OptionSelectableElementWrapperAtomExtension[A <: ElementWrapper, B] extends SelectableElementWrapperAtomExtension[A, Option[B]] {

  override protected def select(elem: Elem, selector: Selector[Elem], f: (Elem) => Option[B]): Option[B] = {
    (elem \ selector).headOption.flatMap(f)
  }
}

trait SeqSelectableElementWrapperAtomExtension[A <: ElementWrapper, B] extends SelectableElementWrapperAtomExtension[A, Seq[B]] {
  override protected def select(elem: Elem, selector: Selector[Elem], f: (Elem) => Seq[B]): Seq[B] = {
    (elem \ selector).flatMap(f)
  }
}

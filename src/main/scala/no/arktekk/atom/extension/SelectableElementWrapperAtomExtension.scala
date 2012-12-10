/*
 * Copyright 2012 Arktekk AS
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package no.arktekk.atom.extension

import com.codecommit.antixml.{Elem, Selector}
import no.arktekk.atom.{ElementWrapper}

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
trait SelectableElementWrapperAtomExtension[A <: ElementWrapper, B, C] extends AtomExtension[A, C] {
  protected def selector: Selector[Elem]
  protected def function: (Elem) => B

  protected def select(elem: Elem, selector: Selector[Elem], f: (Elem) => B): C

  def fromLike(like: A) = select(like.wrapped, selector, function)
}

trait OptionSelectableElementWrapperAtomExtension[A <: ElementWrapper, B] extends SelectableElementWrapperAtomExtension[A, B, Option[B]] {

  override protected def select(elem: Elem, selector: Selector[Elem], f: (Elem) => B): Option[B] = {
    (elem \ selector).headOption.map(f)
  }
}

trait SeqSelectableElementWrapperAtomExtension[A <: ElementWrapper, B] extends SelectableElementWrapperAtomExtension[A, B, IndexedSeq[B]] {
  override protected def select(elem: Elem, selector: Selector[Elem], f: (Elem) => B): IndexedSeq[B] = {
    (elem \ selector).map(f)
  }
}

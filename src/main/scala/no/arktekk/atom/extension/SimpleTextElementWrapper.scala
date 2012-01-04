/*
 * Copyright 2011 Arktekk AS
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

import no.arktekk.atom.{Namespaced, ElementWrapper}
import com.codecommit.antixml._

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
class SimpleTextElementWrapper(elem: Elem) extends ElementWrapper {

  type T = SimpleTextElementWrapper

  protected val self = this

  def wrapped = elem

  def copy(elem: Elem) = new SimpleTextElementWrapper(elem)

  def value = (wrapped \ text).head

  def addAttribute(name: QName, value: String) = copy(elem.copy(attrs = elem.attrs + (name -> value)))

  def removeAttribute(name: QName) = copy(elem.copy(attrs = elem.attrs - name))

  def withValue(text: String) = copy(elem.copy(children = Group(Text(text))))
}

object SimpleTextElementWrapper {
  def apply(namespaced: Namespaced, value: String): SimpleTextElementWrapper = {
    new SimpleTextElementWrapper(Elem(namespaced.prefix, namespaced.name, Attributes(), namespaced.toMap, Group(Text(value))))
  }

  def unapply(e: SimpleTextElementWrapper) = Some(e.wrapped)
}

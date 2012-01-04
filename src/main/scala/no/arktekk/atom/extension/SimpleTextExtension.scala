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

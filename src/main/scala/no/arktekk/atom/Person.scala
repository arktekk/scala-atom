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
package no.arktekk.atom


import com.codecommit.antixml.Selector._
import com.codecommit.antixml._
import extension.SimpleTextElementWrapper

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
case class Person private[atom](wrapped: Elem) extends ElementWrapper {

  type T = Person

  val self = this

  def copy(elem: Elem) = new Person(elem)

  def name = (wrapped \ "name" \ text).head

  def email = (wrapped \ "email" \ text).headOption

  def url = (wrapped \ "url" \ text).headOption
}

object Person {
  def author(name: String): Person = Person(
    ElementWrapper.withName(NamespacedName(Atom.namespace, "author")).
      addChild(SimpleTextElementWrapper(NamespacedName(Atom.namespace, "name"), name)).wrapped
  )

  def contributor(name: String): Person = Person(
    ElementWrapper.withName(NamespacedName(Atom.namespace, "contributor")).
       addChild(SimpleTextElementWrapper(NamespacedName(Atom.namespace, "name"), name)).wrapped
  )
}

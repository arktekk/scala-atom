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

import com.codecommit.antixml._

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
case class Category private[atom](wrapped: Elem) extends ElementWrapper {
  require(Elem.validateNamespace(wrapped, Atom.namespace), "Wrong namespace defined")
  require(wrapped.name == "category", "Wrong name of element")

  type T = Category

  protected val self = this

  def scheme = wrapped.attr("scheme")

  def term = wrapped.attr("term").get

  def label = wrapped.attr("label")

  def copy(elem: Elem) = new Category(elem)
}

object Category {
  def apply(scheme: Option[String], term: String, label: Option[String]): Category = {
    val attrs = Attributes() ++ scheme.map((QName("scheme") -> _.toString)) + ("term" -> term) ++ label.map((QName("label") -> _))
    Category(Elem(NamespaceBinding(Atom.namespace), "category", attrs))
  }
}

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

import collection.immutable.Map
import Atom._
import com.codecommit.antixml._

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
case class Category private[atom](wrapped: Elem) extends Extensible {
  def scheme = wrapped.attrs.get("scheme")

  def term = wrapped.attrs.get("term").get

  def label = wrapped.attrs.get("label")

  type A = Category

  def copy(wrapped: Elem) = copy(wrapped = wrapped)
}

object Category {
  def apply(scheme: Option[String], term: String, label: Option[String]): Category = {
    val attrs = new Attributes(
      Map[QName, String]() ++
        scheme.map((QName(None, "scheme") -> _.toString)) +
        ("term" -> term) ++
        label.map((QName(None, "label") -> _))
    )
    Category(onlyElementName("category").copy(attrs = attrs))
  }
}

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

import java.net.URI
import collection.immutable.Map
import com.codecommit.antixml._

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
case class Generator private[atom](wrapped: Elem) {
  require(Elem.validateNamespace(wrapped, Atom.namespace), "Wrong namespace defined")
  require(wrapped.name == "generator")

  def uri = wrapped.attrs.get("uri").map(URI.create(_))

  def version = wrapped.attrs.get("version")

  def value = (wrapped \ text).head
}

object Generator {
  def apply(uri: Option[URI], version: Option[String], value: String): Generator = {
    val attr = Attributes() ++ uri.map((QName("uri") -> _.toString)) ++ version.map((QName("version") -> _))
    Generator(Elem(Atom.atom, "generator", attr).addChild(Text(value)))
  }
}

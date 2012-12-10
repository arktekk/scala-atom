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
case class Generator private[atom](wrapped: Elem) extends ElementWrapper {
  require(Elem.validateNamespace(wrapped, Atom.namespace), "Wrong namespace defined")
  require(wrapped.name == "generator")


  type T = Generator

  protected def self = this

  def copy(elem: Elem) = new Generator(elem)

  lazy val uri = wrapped.attrs.get("uri").map(URI.create(_))

  lazy val version = wrapped.attrs.get("version")

  lazy val value = (wrapped \ text).head

  def withURI(href: URI) = withAttribute("uri", href.toString)

  def withVersion(version: String) = withAttribute("version", version)

  def withValue(value: String) = withChildren(Group[Node](Text(value)))
}

object Generator {
  def apply(uri: Option[URI], version: Option[String], value: String): Generator = {
    val attr = Attributes() ++ uri.map((QName("uri") -> _.toString)) ++ version.map((QName("version") -> _))
    Generator(Elem(Atom.atom, "generator", attr).addChild(Text(value)))
  }

  def apply(uri: URI, version: String, value: String): Generator = {
    apply(Some(uri), Some(version), value)
  }
}

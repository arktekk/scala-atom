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
import com.codecommit.antixml._

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
case class Link(wrapped: Elem) extends ElementWrapper {
  require(Elem.validateNamespace(wrapped, Atom.namespace), "Wrong namespace defined")
  require(wrapped.name == "link")

  type T = Link

  protected val self = this

  def copy(wrapped: Elem) = new Link(wrapped)

  def href: URI = wrapped.attrs.get("href").map(URI.create(_)).getOrElse(throw new IllegalStateException("href is required"))

  def rel: Option[String] = wrapped.attrs.get("rel")

  def mediaType: Option[MediaType] = wrapped.attrs.get("type").flatMap(MediaType(_))

  def title: Option[String] = wrapped.attrs.get("title")

  def withTitle(title: String): Link = withAttribute("title", title)

  def withHref(href: URI): Link = withAttribute("href", href.toString)

  def withRel(rel: String): Link = withAttribute("rel", rel)

  def withMediaType(mt: MediaType): Link = withAttribute("type", mt.toString)
}


object Link {
  private def toAttributes[A](name: String, option: Option[A]) = {
    option.map(x => (QName(name) -> x.toString))
  }

  def apply(href: URI, rel: String, mediaType: Option[MediaType] = None, title: Option[String] = None): Link = {
    val attrs = List[(QName, String)](("href" -> href.toString), ("rel" -> rel)) ++ toAttributes("type", mediaType) ++ toAttributes("title", title)
    Link(Elem(Atom.atom, "link", Attributes(attrs : _*)))
  }
}

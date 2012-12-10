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

package no.arktekk.atom

import com.codecommit.antixml._
import java.net.URI


/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
case class Collection(wrapped: Elem) extends ElementWrapper {
  require(Elem.validateNamespace(wrapped, Atom.atompubNamespace), "Wrong namespace defined")
  require(wrapped.name == "collection", "Wrong name of element")

  def href: URI = wrapped.attrs.get("href").map(URI.create(_)).get

  def title: Option[TextConstruct] = (wrapped \ atomSelector("title")).headOption.flatMap(TextConstruct(_))

  def accepts: IndexedSeq[MediaType] = (wrapped \ atomPubSelector("accept") \ text).flatMap(MediaType(_).toSeq)

  def withHref(uri: URI) = withAttribute("href", uri.toString)

  def withTitle(text: TextConstruct) = {
    replaceChildren(atomSelector("title"), text.toXML("title", Some("atom")).toGroup)
  }

  def addAccept(mt: MediaType) = addChild(ElementWrapper.withNameAndText(
    NamespaceBinding("app", Atom.atompubNamespace), "accept", mt.toString
  ))

  def withAccepts(accepts: IndexedSeq[MediaType]) = {
    replaceChildren(atomPubSelector("accept"), Group.fromSeq(accepts.map(a => Elem(NamespaceBinding("app", Atom.atompubNamespace), "accept", Attributes(), Group[Node](Text(a.toString))))))
  }

  type T = Collection

  protected def self = this

  def copy(elem: Elem) = new Collection(elem)
}

object Collection {
  def apply(): Collection = apply(Elem(NamespaceBinding("app", Atom.atompubNamespace), "collection"))

  def apply(href: URI): Collection = apply().withHref(href)
}

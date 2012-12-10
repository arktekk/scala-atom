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

import com.codecommit.antixml.{Attributes, Node, Group, Elem}
import java.net.URI
import org.joda.time.DateTime

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
case class Source(wrapped: Elem) extends FeedLike {
  require(Elem.validateNamespace(wrapped, Atom.namespace), "Wrong namespace defined")
  require(wrapped.name == "source", "Wrong name of element")

  type T = Source

  protected val self = this

  def copy(elem: Elem) = new Source(elem)
}

object Source {
  def apply(
             id: URI,
             title: TextConstruct,
             updated: DateTime,
             links: IndexedSeq[Link],
             authors: IndexedSeq[Person],
             subtitle: Option[TextConstruct] = None,
             generator: Option[Generator] = None,
             logo: Option[URI] = None,
             icon: Option[URI] = None,
             rights: Option[TextConstruct] = None,
             contributors: IndexedSeq[Person] = IndexedSeq.empty
             ): Source = {
    val group = Group[Node](
      atomTextElem("id", id.toString),
      atomTextElem("updated", dateTimeToString(updated)),
      title.toXML("title")
    ) ++
      subtitle.map(_.toXML("subtitle")) ++
      authors.map(_.wrapped) ++
      contributors.map(_.wrapped) ++
      links.map(_.wrapped) ++
      generator.map(_.wrapped) ++
      logo.map(u => atomTextElem("logo", u.toString)) ++
      icon.map(u => atomTextElem("icon", u.toString)) ++
      rights.map(_.toXML("rights"))

    Source(Elem(Atom.atom, "source", Attributes(), group))
  }

}
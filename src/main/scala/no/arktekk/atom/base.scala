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

import java.net.URI
import org.joda.time.DateTime
import java.util.UUID
import com.codecommit.antixml._

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
sealed trait Base extends AtomLike {

  type T <: Base

  protected def self: T

  override def toString = wrapped.toString()

  protected def isValid(elem: Elem, name: String) = Elem.validateNamespace(elem, Atom.namespace) && elem.name == name
}

private [atom] object BaseBuilder {
  def apply(name:String, id: URI, title: TextConstruct, updated: DateTime, author: Option[Person] = None): Elem = {
    Elem(Atom.atom, name, Attributes(), Group.fromSeq(IndexedSeq(
      Elem(Atom.atom, "id", Attributes(), Group[Node](Text(id.toString))),
      title.toXML("title"),
      Elem(Atom.atom, "updated", Attributes(), Group[Node](Text(dateTimeToString(updated))))
    ) ++ author.map(_.wrapped)))
  }

  def simpleTextElem(name: String, text: String) =
    Elem(Atom.atom, name, Attributes(), Group[Node](Text(text)))
}

case class Feed private[atom](wrapped: Elem) extends Base with FeedLike {
  require(isValid(wrapped, "feed"))

  type T = Feed

  protected val self = this

  def copy(elem: Elem) = new Feed(elem)

  override lazy val id = super.id

  override lazy val title = super.title

  override lazy val subtitle = super.subtitle

  override lazy val updated = super.updated

  override lazy val entries = super.entries

  override lazy val logo = super.logo

  override lazy val icon = super.icon

  override lazy val rights = super.rights

  override lazy val authors = super.authors

  override lazy val contributors = super.contributors

  override lazy val categories = super.categories

  override lazy val links = super.links
}

object Feed {
  def apply(id: URI, title: TextConstruct, updated: DateTime, author: Person): Feed = {
    Feed(BaseBuilder("feed", id, title, updated, Some(author)))
  }

  def apply(title: String, updated: DateTime, author: Person): Feed = {
    Feed(URI.create("urn:uuid:%s".format(UUID.randomUUID().toString)), title, updated, author)
  }

  def apply(
             id: URI,
             title: TextConstruct,
             updated: DateTime,
             links: IndexedSeq[Link],
             authors: IndexedSeq[Person],
             entries: IndexedSeq[Entry],
             subtitle: Option[TextConstruct] = None,
             logo: Option[URI] = None,
             icon: Option[URI] = None,
             rights: Option[TextConstruct] = None,
             contributors: IndexedSeq[Person] = IndexedSeq.empty
             ): Feed = {
    import BaseBuilder._
    val group = Group[Node](
      simpleTextElem("id", id.toString),
      simpleTextElem("updated", dateTimeToString(updated)),
      title.toXML("title")
    ) ++
      subtitle.map(_.toXML("subtitle")) ++
      authors.map(_.wrapped) ++
      contributors.map(_.wrapped) ++
      links.map(_.wrapped) ++
      entries.map(_.wrapped) ++
      logo.map(u => simpleTextElem("logo", u.toString)) ++
      icon.map(u => simpleTextElem("icon", u.toString)) ++
      rights.map(_.toXML("rights"))

    Feed(Elem(Atom.atom, "feed", Attributes(), group))
  }
}

case class Entry private[atom](wrapped: Elem) extends Base with EntryLike {
  require(isValid(wrapped, "entry"))

  type T = Entry

  protected val self = this

  def copy(elem: Elem) = new Entry(elem)

  override lazy val id = super.id

  override lazy val title = super.title

  override lazy val rights = super.rights

  override lazy val updated = super.updated

  override lazy val authors = super.authors

  override lazy val contributors = super.contributors

  override lazy val categories = super.categories

  override lazy val links = super.links

  override lazy val published = super.published

  override lazy val content = super.content

  override lazy val summary = super.summary
}

object Entry {
  def apply(id: URI, title: TextConstruct, updated: DateTime, author: Option[Person]): Entry = {
    Entry(BaseBuilder("entry", id, title, updated, author))
  }

  def apply(id: URI, title: String, updated: DateTime): Entry = {
    apply(id, TextConstruct.Textual(title), updated, None)
  }

  def apply(id: URI,
            title: TextConstruct,
            updated: DateTime,
            published: DateTime,
            links: IndexedSeq[Link],
            authors: IndexedSeq[Person],
            rights: Option[TextConstruct] = None,
            summary: Option[Content] = None,
            content: Option[Content] = None,
            source: Option[Source] = None,
            contributors: IndexedSeq[Person] = IndexedSeq.empty
             ): Entry = {

    import BaseBuilder._

    val group = Group[Node](
      simpleTextElem("id", id.toString),
      title.toXML("title"),
      simpleTextElem("updated", dateTimeToString(updated)),
      simpleTextElem("published", dateTimeToString(published))
    ) ++
      authors.map(_.wrapped) ++
      contributors.map(_.wrapped) ++
      links.map(_.wrapped) ++
      summary.map(_.toXML("summary")) ++
      content.map(_.toXML("content")) ++
      source.map(_.wrapped) ++
      rights.map(_.toXML("rights"))

    Entry(Elem(Atom.atom, "entry", Attributes(), group))
  }

}

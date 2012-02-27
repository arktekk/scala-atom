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

  protected def isValid(elem: Elem, name: String) = elem match {
    case Elem(None, `name`, _, scope, _) if (!scope.isEmpty) => scope("") == Atom.namespace
    case Elem(Some(ns),`name`, _, scope, _) if (!scope.isEmpty) => scope(ns) == Atom.namespace
    case _ => false
  }
}

case class Feed private[atom](wrapped: Elem) extends Base with FeedLike {
  require(isValid(wrapped, "feed"))

  type T = Feed

  protected val self = this

  def copy(elem: Elem) = new Feed(elem)

  def withEntries(entries: Seq[Entry]) = {
    val toCopy = removeChildren("entry")
    copy(toCopy.copy(children = toCopy.children ++ entries.map(_.wrapped)))
  }

  def addEntry(entry: Entry) = {
    copy(wrapped.copy(children = wrapped.children ++ List(entry.wrapped)))
  }
}

object Feed {
  def apply(id: URI, title: TextConstruct, updated: DateTime, author: Person): Feed = {
    val elem = withChildren("feed", children = Group(
      simple("id", id.toString), title.toXML("title"), simple("updated", dateTimeToString(updated))
    ))
    Feed(elem).addAuthor(author)
  }

  def apply(title: String, updated: DateTime, author: Person): Feed = {
    Feed(URI.create("urn:uuid:%s".format(UUID.randomUUID().toString)), title, updated, author)
  }
}

case class Entry private[atom](wrapped: Elem) extends Base with EntryLike {
  require(isValid(wrapped, "entry"))

  type T = Entry

  protected val self = this

  def copy(elem: Elem) = new Entry(elem)

}

object Entry {
  def apply(id: URI, title: String, updated: DateTime): Entry = {
    Entry(onlyElementName("entry").copy(children = Group(simple("id", id.toString), simple("title", title), simple("updated", dateTimeToString(updated)))))
  }
}

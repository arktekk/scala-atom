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
import org.joda.time.DateTime
import java.util.UUID
import Atom._
import com.codecommit.antixml.Selector._
import java.io._
import java.nio.charset.Charset
import com.codecommit.antixml._

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
sealed trait Base extends AtomLike {

  type A <: Base

  protected def self: A

  override def toString = wrapped.toString()

  def addNamespace(prefix: String, namespace: String): A  = addNamespace((prefix.trim(), namespace.trim()))

  def addNamespace(prefixNS: (String, String)): A = {
    def nextValidPrefix = {
      var i = 1
      while (wrapped.scope.contains("ns" + i)) {
        i = i + 1
      }
      "ns" + i
    }
    val currentNS = wrapped.scope

    prefixNS match {
      case (x, y) if (!currentNS.filter{case (_, z) => z == y}.isEmpty) => self
      case ("", y) => {
        val p = nextValidPrefix
        copy(wrapped.copy(scope = currentNS + (p -> y)))
      }
      case (x, y) => copy(wrapped.copy(scope = currentNS +(x -> y)))
    }
  }

  def writeTo(writer: Writer)(implicit charset: Charset) {
    XMLSerializer(charset.name(), true).serializeDocument(wrapped, writer)
  }

  def writeTo(stream: OutputStream)(implicit charset: Charset) {
    writeTo(new OutputStreamWriter(stream, charset))(charset)
  }

  def writeTo(file: File)(implicit charset: Charset) {
    writeTo(new FileWriter(file))(charset)
  }
}

case class Feed private[atom](wrapped: Elem) extends Base with FeedLike {
  require(wrapped.name == "feed")

  type A = Feed

  protected val self = this

  def copy(elem: Elem) = new Feed(elem)

  def withEntries(entries: Seq[Entry]) = {
    val matcher: PartialFunction[Node, Elem] = {
      case x: Elem => x
    }
    val toCopy = (wrapped \ "entry").take(0).unselect.headOption.map(matcher).getOrElse(wrapped)
    copy(toCopy.copy(children = toCopy.children ++ entries.map(_.wrapped)))
  }

  def addEntry(entry: Entry) = {
    copy(wrapped.copy(children = wrapped.children ++ List(entry.wrapped)))
  }
}

object Feed {
  def apply(id: URI, title: TextConstruct, updated: DateTime, author: Person): Feed = {
    val elem = Elem(None, "feed", Attributes(), namespaces, children = Group(
      simple("id", id.toString), title.toXML("title"), simple("updated", dateTimeFormat.print(updated))
    ))
    Feed(elem).addAuthor(author)
  }

  def apply(title: String, updated: DateTime, author: Person): Feed = {
    Feed(URI.create("urn:uuid:%s".format(UUID.randomUUID().toString)), title, updated, author)
  }
}

case class Entry private[atom](wrapped: Elem) extends Base with EntryLike {
  require(wrapped.name == "entry")

  type A = Entry

  protected val self = this

  def copy(elem: Elem) = new Entry(elem)

}

object Entry {
  def apply(id: URI, title: String, updated: DateTime): Entry = {
    Entry(onlyElementName("entry").copy(children = Group(simple("id", id.toString), simple("title", title), simple("updated", dateTimeFormat.print(updated)))))
  }
}

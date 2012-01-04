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


sealed trait Base extends Extensible {

  type A <: Base

  protected def self: A

  def id: URI = (wrapped \ "id" \ text).headOption.map(URI.create(_)).get

  def title: TextConstruct = (wrapped \ "title").headOption.flatMap(TextConstruct(_)).get

  def rights: Option[TextConstruct] = (wrapped \ "rights").headOption.flatMap(TextConstruct(_))

  def updated: DateTime = (wrapped \ "updated" \ text).headOption.map(dateTimeFormat.parseDateTime(_)).get

  def authors: List[Person] = (wrapped \ "author").map(Person(_)).toList

  def contributors: List[Person] = (wrapped \ "contributor").map(Person(_)).toList

  def categories: List[Category] = (wrapped \ "category").map(Category(_)).toList

  def links: List[Link] = (wrapped \ "link").map(Link(_)).toList

  def toXML: Elem = wrapped

  override def toString = toXML.toString()

  def copy(elem: Elem): A

  def withId(id: URI) = copy(removeChild("id").copy(children = wrapped.children ++ List(simple("id", id.toString))))

  def withTitle(title: TextConstruct) = copy(removeChild("title").copy(children = wrapped.children ++ List(title.toXML("title"))))

  def withRights(rights: TextConstruct) = copy(removeChild("rights").copy(children = wrapped.children ++ List(rights.toXML("rights"))))

  def withUpdated(updated: DateTime) = copy(removeChild("updated").copy(children = wrapped.children ++ List(simple("updated", dateTimeFormat.print(updated)))))

  def addAuthor(author: Person) = copy(wrapped.copy(children = wrapped.children ++ List(author.wrapped)))

  def addContributor(contrib: Person) = copy(wrapped.copy(children = wrapped.children ++ List(contrib.wrapped)))

  def addCategory(category: Category) = copy(wrapped.copy(children = wrapped.children ++ List(category.wrapped)))

  def addLink(link: Link) = copy(wrapped.copy(children = wrapped.children ++ List(link.wrapped)))

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

  protected def removeChild(name: String) = {
    val matcher: PartialFunction[Node, Elem] = {
      case x: Elem => x
    }
    (wrapped \ name).take(0).unselect.headOption.map(matcher).getOrElse(wrapped)
  }
}

case class Feed private[atom](wrapped: Elem) extends Base {
  require(wrapped.name == "feed")

  type A = Feed

  protected val self = this

  def subtitle: Option[TextConstruct] = (wrapped \ "subtitle").headOption.flatMap(TextConstruct(_))

  def entries: List[Entry] = (wrapped \ "entry").map(Entry(_)).toList

  def logo = (wrapped \ "logo" \ text).headOption.map(URI.create(_))

  def icon = (wrapped \ "icon" \ text).headOption.map(URI.create(_))

  def withSubtitle(title: TextConstruct) = copy(removeChild("subtitle").copy(children = wrapped.children ++ List(title.toXML("subtitle"))))

  def withLogo(logo: URI) = copy(removeChild("logo").copy(children = wrapped.children ++ List(simple("logo", logo.toString))))

  def withIcon(icon: URI) = copy(removeChild("icon").copy(children = wrapped.children ++ List(simple("icon", icon.toString))))

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

  def copy(elem: Elem) = new Feed(elem)
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

case class Entry private[atom](wrapped: Elem) extends Base {
  require(wrapped.name == "entry")

  type A = Entry

  protected val self = this

  def copy(elem: Elem) = new Entry(elem)

  def published: Option[DateTime] = (wrapped \ "published" \ text).headOption.map(dateTimeFormat.parseDateTime(_))

  def content: Option[Content] = (wrapped \ "content").headOption.flatMap(Content(_))

  def summary: Option[Content] = (wrapped \ "summary").headOption.flatMap(Content(_))

  def withPublished(published: DateTime) = copy(removeChild("published").copy(children = wrapped.children ++ List(simple("published", dateTimeFormat.print(published)))))

  def withSummary(summary: Content) = copy(removeChild("summary").copy(children = wrapped.children ++ List(summary.toXML("summary"))))

  def withContent(content: Content) = copy(removeChild("content").copy(children = wrapped.children ++ List(content.toXML("content"))))

  def removeContent() = copy(removeChild("content"))

  def removeSummary() = copy(removeChild("summary"))
}

object Entry {
  def apply(id: URI, title: String, updated: DateTime): Entry = {
    Entry(onlyElementName("entry").copy(children = Group(simple("id", id.toString), simple("title", title), simple("updated", dateTimeFormat.print(updated)))))
  }
}

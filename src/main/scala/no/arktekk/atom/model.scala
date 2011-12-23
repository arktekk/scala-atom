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
import com.codecommit.antixml._
import collection.immutable.Map
import java.lang.String
import org.joda.time.format.DateTimeFormat
import java.util.UUID
import Atom._
import com.codecommit.antixml.Selector._
import java.io._
import io.Source

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
object Atom {
  val namespace = "http://www.w3.org/2005/Atom"
  val namespaces: Map[String, String] = Map(("", namespace))
  val dateTimeFormat = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss.SS'Z'").withZoneUTC()

  private[atom] def onlyElementName(name: String) = {
    Elem(None, name, Attributes(), namespaces, Group.empty)
  }

  private[atom] def simple(name: String, value: String, attr: Attributes = Attributes()) = {
    withChildren(name, attr, Seq(Text(value)))
  }

  private[atom] def withChildren(name: String, attr: Attributes = Attributes(), children: Seq[Node]) = {
    Elem(None, name, attr, namespaces, Group.empty ++ children)
  }
  
  def parse[A >: Base](src: Source): A = {
    val elem = XML.fromSource(src)
    elem match {
      case e@Elem(_, "feed", _, _, _) => Feed(e)
      case e@Elem(_, "entry", _, _, _) => Entry(e)
      case e => throw new IllegalArgumentException("unknown XML here: %s".format(e))
    }
  }
}

private[atom] sealed trait Extensible

private[atom] sealed trait ExtensibleComponent[A <: Extensible] {

  def findExtension(namespaced: Namespaced): Extension

  def findExtensions(namespaced: Namespaced): List[Extension]

  def addExtension(ex: Extension): A
}

case class Document[A <: Base](root: A, namespaces: Map[String, String] = Map(("", Atom.namespace))) {

  def addNamespace(prefix: String, namespace: String) = {
    val ns = namespaces + (prefix -> namespace)
    copy(root.copy(root.wrapped.copy(scope = ns)), ns)
  }

  def writeTo(writer: Writer)(implicit encoding: String = "UTF-8") {
    XMLSerializer.apply(encoding = encoding, outputDeclaration = true).serializeDocument(root.toXML, writer)
  }

  override def toString = root.toString
}

sealed trait Base extends Extensible {

  type A <: Base

  private[atom] def wrapped: Elem

  def title = (wrapped \ "title").headOption.flatMap(TextConstruct(_)).get

  def updated = (wrapped \ "updated" \ text).headOption.map(dateTimeFormat.parseDateTime(_)).get

  def published = (wrapped \ "published" \ text).headOption.map(dateTimeFormat.parseDateTime(_))

  def authors = (wrapped \ "author").map(Person(_)).toList

  def links = (wrapped \ "link").map(Link(_)).toList

  def content = (wrapped \ "content").headOption.map(Content(_))

  def summary = (wrapped \ "summary").headOption.map(Content(_))

  def toXML: Elem = wrapped

  override def toString = toXML.toString()

  def copy(elem: Elem): A

  def withId(id: IRI) = copy(removeChild("id").copy(children = wrapped.children ++ List(simple("id", id.toString))))

  def withTitle(title: TextConstruct) = copy(removeChild("id").copy(children = wrapped.children ++ List(title.toXML("title"))))

  def withUpdated(updated: DateTime) = copy(removeChild("updated").copy(children = wrapped.children ++ List(simple("updated", dateTimeFormat.print(updated)))))

  def withPublished(published: DateTime) = copy(removeChild("published").copy(children = wrapped.children ++ List(simple("published", dateTimeFormat.print(published)))))

  def addAuthor(author: Person) = copy(wrapped.copy(children = wrapped.children ++ List(author.wrapped)))

  def addLink(link: Link) = copy(wrapped.copy(children = wrapped.children ++ List(link.wrapped)))

  protected def removeChild(name: String) = {
    val matcher: PartialFunction[Node, Elem] = {
      case x:Elem => x
    }
    (wrapped \ name).drop(1).unselect.headOption.map(matcher).getOrElse(wrapped)
  }
}

case class Feed private[atom](wrapped: Elem) extends Base {
  require(wrapped.name == "feed")
  
  type A = Feed

  def entries = (wrapped \ "entry").map(Entry(_)).toList

  def withEntries(entries: Seq[Entry]) = {
    val matcher: PartialFunction[Node, Elem] = {
      case x:Elem => x
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
  def apply(id: IRI, title: TextConstruct, updated: DateTime, author: Person): Feed = {
    val elem = Elem(None, "feed", Attributes(), namespaces, children = Group(
      simple("id", id.toString), title.toXML("title"), simple("updated", dateTimeFormat.print(updated))
    ))
    Feed(elem).addAuthor(author)
  }

  def apply(title: String, updated: DateTime, author: Person): Feed = {
    Feed(IRI("urn:uuid:%s".format(UUID.randomUUID().toString)), title, updated, author)
  }
}

case class Entry private[atom](wrapped: Elem) extends Base {
  require(wrapped.name == "entry")

  type A = Entry

  def copy(elem: Elem) = new Entry(elem)

  def withSummary(summary: Content) = copy(removeChild("summary").copy(children = wrapped.children ++ List(summary.toXML("summary"))))

  def withContent(content: Content) = copy(removeChild("content").copy(children = wrapped.children ++ List(content.toXML("content"))))

  def removeContent() = copy(removeChild("content"))

  def removeSummary() = copy(removeChild("summary"))
}

object Entry {
  def apply(id: IRI, title: String, updated: DateTime): Entry = {
    Entry(onlyElementName("entry").copy(children = Group(simple("id", id.toString), simple("title", title), simple("updated", dateTimeFormat.print(updated)))))
  }
}


case class Person private[atom](wrapped: Elem) {
  def name = (wrapped \ "name" \ text).head

  def email = (wrapped \ "email" \ text).headOption

  def url = (wrapped \ "url" \ text).headOption
}

object Person {
  def author(name: String): Person = Person(
    onlyElementName("author").copy(children = Group(simple("name", name)))
  )

  def contributor(name: String): Person = Person(
    onlyElementName("contributor").copy(children = Group(simple("name", name)))
  )
}

case class Link private[atom](wrapped: Elem) {
  require(wrapped.name == "link")
  def href = wrapped.attrs.get("href").map(URI.create(_)).get

  def rel = wrapped.attrs.get("rel")

  def mediaType = wrapped.attrs.get("type").map(MediaType(_))

  def title = wrapped.attrs.get("title")
}

object Link {
  private def toAttributes[A](name: String, option: Option[A]) = {
    option.map(x => (QName(None, name) -> x.toString)).toList
  }

  def apply(href: URI, rel: String, mediaType: Option[MediaType] = None, title: Option[String] = None): Link = {
    val attrs = List[(QName, String)](("href" -> href.toString), ("rel" -> rel)) ++ toAttributes("type", mediaType) ++ toAttributes("title", title)
    Link(onlyElementName("link").copy(attrs = Attributes() ++ attrs))
  }
}

case class IRI(underlying: URI) {
  override def toString = underlying.toString
}

object IRI {
  def apply(string: String): IRI = IRI(URI.create(string))
}

sealed trait Content {
  private[atom] def toXML(name: String): Elem
}

object Content {

  def apply(elem: Elem): Option[Content] = {
    TextConstruct(elem).map(Text(_)).orElse(
      elem.attrs.get("href").map(x => External(URI.create(x), elem.attrs.get("type").map(MediaType(_))))
    ).orElse(elem.attrs.get("type").map(MediaType(_)).map(x => Inline(x, elem.children.head.asInstanceOf[Elem])))
  }

  case class Text(text: TextConstruct) extends Content {
    private[atom] def toXML(name: String) = withChildren(name, Attributes((QName(None, "type"), text.textType.value)), Seq(text.value))
  }

  case class Inline(mediaType: MediaType, elem: Elem) extends Content {
    private[atom] def toXML(name: String) = withChildren(name, Attributes((QName(None, "type"), mediaType.toString)), Seq(elem))
  }

  case class External(href: URI, mediaType: Option[MediaType]) extends Content {
    private[atom] def toXML(name: String) = onlyElementName(name).copy(attrs = Attributes(("href" -> href.toString)) ++ mediaType.foldLeft(Map[QName, String]())((acc, mt) => acc + ("type" -> mt.toString)))
  }

}

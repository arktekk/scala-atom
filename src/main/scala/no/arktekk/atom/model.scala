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
import collection.immutable.Map
import java.lang.String
import org.joda.time.format.DateTimeFormat
import java.util.UUID
import Atom._
import com.codecommit.antixml.Selector._
import java.io._
import io.{Codec, Source}
import java.nio.charset.Charset
import com.codecommit.antixml._

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
  
  def parse[A <: Base](src: Source): A = {
    val elem = XML.fromSource(src)
    elem match {
      case e@Elem(_, "feed", _, _, _) if (e.scope.find{case (_, ns) => ns == namespace}.isDefined) => Feed(e).asInstanceOf[A]
      case e@Elem(_, "entry", _, _, _) if (e.scope.find{case (_, ns) => ns == namespace}.isDefined) => Entry(e).asInstanceOf[A]
      case e => throw new IllegalArgumentException("unknown XML here: %s".format(e))
    }
  }
}

case class Document[A <: Base](root: A, namespaces: Map[String, String] = Map(("", Atom.namespace))) {

  def addNamespace(prefix: String, namespace: String) = {
    val ns = namespaces + (prefix -> namespace)
    copy(root.copy(root.wrapped.copy(scope = ns)), ns)
  }

  def writeTo(writer: Writer)(implicit codec: Charset = Codec.UTF8) {
    XMLSerializer(codec.name(), true).serializeDocument(root.toXML, writer)
  }

  override def toString = root.toString
}

sealed trait Base extends Extensible {

  type A <: Base

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

  def subtitle: Option[TextConstruct] = (wrapped \ "subtitle").headOption.flatMap(TextConstruct(_))

  def entries: List[Entry] = (wrapped \ "entry").map(Entry(_)).toList

  def logo = (wrapped \ "logo" \ text).headOption.map(URI.create(_))

  def icon = (wrapped \ "icon" \ text).headOption.map(URI.create(_))

  def withSubtitle(title: TextConstruct) = copy(removeChild("subtitle").copy(children = wrapped.children ++ List(title.toXML("subtitle"))))

  def withLogo(logo: URI) = copy(removeChild("logo").copy(children = wrapped.children ++ List(simple("logo", logo.toString))))

  def withIcon(icon: URI) = copy(removeChild("icon").copy(children = wrapped.children ++ List(simple("icon", icon.toString))))

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

case class Category private[atom](wrapped: Elem) extends Extensible {
  def scheme = wrapped.attrs.get("scheme")
  def term = wrapped.attrs.get("term").get
  def label = wrapped.attrs.get("label")

  type A = Category

  def copy(wrapped: Elem) = copy(wrapped = wrapped)
}

object Category {
  def apply(scheme: Option[String], term: String, label: Option[String]): Category = {
    val attrs = new Attributes(
      Map[QName, String]() ++
        scheme.map((QName(None, "scheme") -> _.toString)) +
        ("term" -> term) ++
        label.map((QName(None, "label") -> _))
    )
    Category(onlyElementName("category").copy(attrs = attrs))
  }
}

case class Generator private[atom](wrapped: Elem) {

  def uri = wrapped.attrs.get("uri").map(URI.create(_))

  def version = wrapped.attrs.get("version")

  def value = (wrapped \ text).head
}

object Generator {
  def apply(uri: Option[URI], version: Option[String], value: String): Generator = {
    val attr = new Attributes(Map[QName, String]() ++ uri.map((QName(None, "uri") -> _.toString)) ++ version.map((QName(None, "version") -> _)))
    Generator(simple("generator", value, attr))
  }
}

case class Person private[atom](wrapped: Elem) extends Extensible {

  type A = Person

  def copy(wrapped: Elem) = copy(wrapped = wrapped)

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

case class Link private[atom](wrapped: Elem) extends Extensible {
  require(wrapped.name == "link")

  type A = Link

  def copy(wrapped: Elem) = copy(wrapped = wrapped)

  def href = wrapped.attrs.get("href").map(URI.create(_)).get

  def rel = wrapped.attrs.get("rel")

  def mediaType: Option[MediaType ] = wrapped.attrs.get("type").flatMap(MediaType(_))

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

sealed trait Content {
  private[atom] def toXML(name: String): Elem
}

object Content {

  def apply(elem: Elem): Option[Content] = {
    val mediaType = elem.attrs.get("type").flatMap(MediaType(_))
    mediaType match {
      case mt@Some(_) if (elem.attrs.contains("href")) => Some(External(URI.create(elem.attrs("href")), mt))
      case Some(mt) => Some(Inline(mt, elem.children.head.asInstanceOf[Elem]))
      case None => TextConstruct(elem).map(Text(_))
    }
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

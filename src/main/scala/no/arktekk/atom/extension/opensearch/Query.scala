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
package no.arktekk.atom.extension.opensearch

import java.util.Locale
import java.nio.charset.Charset
import no.arktekk.atom.extension.AtomExtension
import com.codecommit.antixml.{Group, Attributes, Elem}
import no.arktekk.atom.{FeedLike, Atom, ElementWrapper}
import no.arktekk.atom.extension.opensearch.OpensearchConstants._

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
case class Query(role: Role, attributes: Attributes = Attributes(), namespaces: Map[String, String] = Map((defaultPrefix -> openSearchNamespace))) {
  def title = attributes.get("title")

  def searchTerms = attributes.get("searchTerms")

  def count = attributes.get("count").map(_.toInt)

  def totalResults = attributes.get("totalResults").map(_.toInt)

  def startIndex = attributes.get("startIndex").map(_.toInt)

  def startPage = attributes.get("startPage").map(_.toInt)

  def inputEncoding = attributes.get("inputEncoding").map(Charset.forName(_))

  def outputEncoding = attributes.get("outputEncoding").map(Charset.forName(_))

  def getAttribute(name: String) = attributes.get(name)

  def withRole(role: Role) = copy(role)

  def withTitle(title: String) = withAttribute("title", title)

  def withSearchTerms(terms: String) = withAttribute("searchTerms", terms)
  
  def withCount(count: Int) = withAttribute("count", count.toString)
  
  def withTotalResults(results: Int) = withAttribute("totalResults", results.toString)
  
  def withStartIndex(index: Int) = withAttribute("startIndex", index.toString)
  
  def withStartPage(page: Int) = withAttribute("startPage", page.toString)
  
  def withInputEncoding(encoding: Charset) = withAttribute("inputEncoding", encoding.name())
  
  def withOutputEncoding(encoding: Charset) = withAttribute("outputEncoding", encoding.name())
  
  def withAttribute(name: String, value: String) = copy(attributes = attributes + (name -> value))
  
  def addNamespace(prefix: String,  namespace: String) = copy(namespaces = namespaces + (prefix -> namespace))
  
  def toElem = Elem(Some(defaultPrefix), "query", attributes + ("role" -> role.name), namespaces, Group.empty)
}

object Query {
  def apply(elem: Elem): Query = {
    val role = elem.attrs.get("role").map(Role(_)).getOrElse(Role.REQUEST)
    Query(role, elem.attrs)
  }
}

object QueryAtomExtension extends AtomExtension[FeedLike, Seq[Query]] {
  def fromLike(like: FeedLike) = (like.wrapped \ Atom.namespaceSelector(openSearchNamespace, "query")).map(_.asInstanceOf[Elem]).map(Query(_))

  def toElem(a: Seq[Query]) = a.map(_.toElem).map(ElementWrapper(_))
}

class Role(val name: String) {
  override def hashCode() = name.hashCode()

  override def equals(obj: Any) = obj match {
    case Role(n) => n == name
    case _ => false
  }

  override def toString = name
}

object Role {
  val REQUEST = new Role("request")
  val EXAMPLE = new Role("example")
  val RELATED = new Role("related")
  val CORRECTION = new Role("correction")
  val SUBSET = new Role("subset")
  val SUPERSET = new Role("superset")

  private val map = Map(
    REQUEST.name -> REQUEST,
    EXAMPLE.name -> EXAMPLE,
    RELATED.name -> RELATED,
    CORRECTION.name -> CORRECTION,
    SUBSET.name -> SUBSET,
    SUPERSET.name -> SUPERSET
  )

  def apply(name: String) = {
    val lower = name.toLowerCase(Locale.ENGLISH)
    map.get(lower).getOrElse(new Role(lower))
  }

  def unapply(role: Role) = Some(role.name)
}

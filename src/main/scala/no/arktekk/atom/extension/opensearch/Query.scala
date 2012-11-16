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
import com.codecommit.antixml.{Group, Attributes, Elem}
import no.arktekk.atom.extension.opensearch.OpensearchConstants._
import no.arktekk.atom._

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
case class Query private[opensearch](wrapped: Elem) extends ElementWrapper{

  type T = Query

  protected def self = this

  def copy(elem: Elem) = new Query(elem)

  def role = getAttribute("role").map(Role(_)).get

  def title = getAttribute("title")

  def searchTerms = getAttribute("searchTerms")

  def count = getAttribute("count").map(_.toInt)

  def totalResults = getAttribute("totalResults").map(_.toInt)

  def startIndex = getAttribute("startIndex").map(_.toInt)

  def startPage = getAttribute("startPage").map(_.toInt)

  def inputEncoding = getAttribute("inputEncoding").map(Charset.forName(_))

  def outputEncoding = getAttribute("outputEncoding").map(Charset.forName(_))

  def withRole(role: Role) = withAttribute("role", role.name)

  def withTitle(title: String) = withAttribute("title", title)

  def withSearchTerms(terms: String) = withAttribute("searchTerms", terms)

  def withCount(count: Int) = withAttribute("count", count.toString)

  def withTotalResults(results: Int) = withAttribute("totalResults", results.toString)

  def withStartIndex(index: Int) = withAttribute("startIndex", index.toString)

  def withStartPage(page: Int) = withAttribute("startPage", page.toString)

  def withInputEncoding(encoding: Charset) = withAttribute("inputEncoding", encoding.name())

  def withOutputEncoding(encoding: Charset) = withAttribute("outputEncoding", encoding.name())

  def getAttribute(name: String) = wrapped.attrs.get(name)
}

object Query {
  val selector = namespaceSelector(ns, "Query")

  def apply(role: Role): Query = {
    Query(ElementWrapper.withNameAndAttributes(NamespacedName(ns, prefix, "Query"), Attributes("role" -> role.name)).wrapped)
  }

  def apply(): Query = {
    apply(Role.REQUEST)
  }
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

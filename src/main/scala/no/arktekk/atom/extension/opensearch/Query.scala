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

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
trait Query  {

  def role: Role

  def title: Option[String]

  def searchTerms: Option[String]

  def count: Option[Int]

  def totalResults: Option[Int]

  def startIndex: Option[Int]

  def startPage: Option[Int]

  def inputEncoding: Option[Charset]

  def outputEncoding: Option[Charset]

  def withRole(role: Role): Query

  def withTitle(title: String): Query

  def withSearchTerms(terms: String):Query

  def withCount(count: Int):Query

  def withTotalResults(results: Int): Query

  def withStartIndex(index: Int):Query

  def withStartPage(page: Int):Query

  def withInputEncoding(encoding: Charset):Query

  def withOutputEncoding(encoding: Charset):Query

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

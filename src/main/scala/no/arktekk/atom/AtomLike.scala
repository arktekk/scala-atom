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
import no.arktekk.atom.Atom._
import com.codecommit.antixml._

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
private[atom] trait AtomLike {
  type T <: AtomLike

  def wrapped: Elem

  def id: URI = (wrapped \ "id" \ text).headOption.map(URI.create(_)).get

  def title: TextConstruct = (wrapped \ "title").headOption.flatMap(TextConstruct(_)).get

  def rights: Option[TextConstruct] = (wrapped \ "rights").headOption.flatMap(TextConstruct(_))

  def updated: DateTime = (wrapped \ "updated" \ text).headOption.map(dateTimeFormat.parseDateTime(_)).get

  def authors: List[Person] = (wrapped \ "author").map(Person(_)).toList

  def contributors: List[Person] = (wrapped \ "contributor").map(Person(_)).toList

  def categories: List[Category] = (wrapped \ "category").map(Category(_)).toList

  def links: List[Link] = (wrapped \ "link").map(Link(_)).toList

  def copy(elem: Elem): T

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
      case x: Elem => x
    }
    (wrapped \ name).take(0).unselect.headOption.map(matcher).getOrElse(wrapped)
  }

}

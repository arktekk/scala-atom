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
import com.codecommit.antixml._

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
private[atom] trait AtomLike extends ElementWrapper {
  protected val atomSelector = namespaceSelector(Atom.namespace, _ : String)

  def id: URI = (wrapped \ atomSelector("id") \ text).headOption.map(URI.create(_)).get

  def title: TextConstruct = (wrapped \ atomSelector("title")).headOption.flatMap(TextConstruct(_)).get

  def rights: Option[TextConstruct] = (wrapped \ atomSelector("rights")).headOption.flatMap(TextConstruct(_))

  def updated: DateTime = (wrapped \ atomSelector("updated") \ text).headOption.map(parseDateTime(_)).get

  def authors: List[Person] = (wrapped \ atomSelector("author")).map(Person(_)).toList

  def contributors: List[Person] = (wrapped \ atomSelector("contributor")).map(Person(_)).toList

  def categories: List[Category] = (wrapped \ atomSelector("category")).map(Category(_)).toList

  def links: List[Link] = (wrapped \ atomSelector("link")).map(Link(_)).toList

  def withId(id: URI) = copy(removeChildren("id").copy(children = wrapped.children ++ List(simple("id", id.toString))))

  def withTitle(title: TextConstruct) = copy(removeChildren("title").copy(children = wrapped.children ++ List(title.toXML("title"))))

  def withRights(rights: TextConstruct) = copy(removeChildren("rights").copy(children = wrapped.children ++ List(rights.toXML("rights"))))

  def withUpdated(updated: DateTime) = copy(removeChildren("updated").copy(children = wrapped.children ++ List(simple("updated", dateTimeToString(updated)))))

  def addAuthor(author: Person) = copy(wrapped.copy(children = wrapped.children ++ List(author.wrapped)))

  def addContributor(contrib: Person) = copy(wrapped.copy(children = wrapped.children ++ List(contrib.wrapped)))

  def addCategory(category: Category) = copy(wrapped.copy(children = wrapped.children ++ List(category.wrapped)))

  def addLink(link: Link) = addLinks(Seq(link))

  def addLinks(toAdd: Seq[Link]) = copy(wrapped.copy(children = wrapped.children ++ toAdd.map(_.wrapped)))

  protected def removeChildren(name: String): Elem = {
    super.removeChildren(atomSelector(name))
  }
  
  def linkByRel(rel: String) = links.find(_.rel == Some(rel))

  def linksByType(mt: MediaType) = links.filter(_.mediaType == Some(mt))

  def authorByName(name: String) = authors.find(_.name == name)

  def contributorByName(name: String) = contributors.find(_.name == name)

  def categoriesByScheme(scheme: String) = categories.filter(_.scheme == Some(scheme))

  def categoryBySchemeAndTerm(scheme: String, term: String) = categories.find(x => x.scheme == Some(scheme) && x.term == term)

}

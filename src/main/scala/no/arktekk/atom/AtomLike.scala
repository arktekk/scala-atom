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

import extension.SimpleTextElementWrapper
import java.net.URI
import org.joda.time.DateTime
import com.codecommit.antixml._

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
private[atom] trait AtomLike extends ElementWrapper {
  protected val atomSelector = namespaceSelector(Atom.namespace, _ : String)

  protected def element(name: String) = (wrapped \ atomSelector(name))
  protected def elementText(name: String) = element(name) \ text

  def id: URI = elementText("id").headOption.map(URI.create(_)).get

  def title: TextConstruct = element("title").headOption.flatMap(TextConstruct(_)).get

  def rights: Option[TextConstruct] = element("rights").headOption.flatMap(TextConstruct(_))

  def updated: DateTime = elementText("updated").headOption.map(parseDateTime(_)).get

  def authors: List[Person] = element("author").map(Person(_)).toList

  def contributors: List[Person] = element("contributor").map(Person(_)).toList

  def categories: List[Category] = element("category").map(Category(_)).toList

  def links: List[Link] = element("link").map(Link(_)).toList

  def withId(id: URI) = copy(removeChildren("id")).addChild("id", id.toString)

  def withTitle(title: TextConstruct) = replaceChildren(atomSelector("title"), title.toXML("title").toGroup)

  def withRights(rights: TextConstruct) = replaceChildren(atomSelector("rights"), rights.toXML("rights").toGroup)

  def withUpdated(updated: DateTime) = copy(removeChildren("updated")).addChild("updated", dateTimeToString(updated))

  def addAuthor(author: Person) = addChild(author)

  def addContributor(contrib: Person) = addChild(contrib)

  def addCategory(category: Category) = addChild(category)

  def addLink(link: Link) = addLinks(Seq(link))

  def addLinks(links: Seq[Link]) = addChildren(links)

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

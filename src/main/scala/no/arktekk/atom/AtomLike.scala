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
  protected def element(name: String) = (wrapped \ atomSelector(name))
  protected def elementText(name: String) = element(name) \ text

  def id: URI = elementText("id").headOption.map(URI.create(_)).get

  def title: TextConstruct = element("title").headOption.flatMap(TextConstruct(_)).get

  def rights: Option[TextConstruct] = element("rights").headOption.flatMap(TextConstruct(_))

  def updated: DateTime = elementText("updated").headOption.map(parseDateTime(_)).get

  def authors: IndexedSeq[Person] = element("author").map(Person(_))

  def contributors: IndexedSeq[Person] = element("contributor").map(Person(_))

  def categories: IndexedSeq[Category] = element("category").map(Category(_))

  def links: IndexedSeq[Link] = element("link").map(Link(_))

  def withId(id: URI): T = replaceChildren(atomSelector("id"), Elem(Atom.atom, "id", Attributes(), Group[Node](Text(id.toString))).toGroup)

  def withTitle(title: TextConstruct): T = replaceChildren(atomSelector("title"), title.toXML("title").toGroup)

  def withRights(rights: TextConstruct): T = replaceChildren(atomSelector("rights"), rights.toXML("rights").toGroup)

  def withUpdated(updated: DateTime): T = replaceChildren(atomSelector("updated"), Elem(Atom.atom, "updated", Attributes(), Group[Node](Text(dateTimeToString(updated)))).toGroup)

  def addAuthor(author: Person): T = addChild(author)

  def addContributor(contrib: Person): T = addChild(contrib)

  def addCategory(category: Category): T = addChild(category)

  def addLink(link: Link): T = addLinks(IndexedSeq(link))

  def addLinks(links: IndexedSeq[Link]): T = addChildren(links)

  def linkByRel(rel: String): Option[Link] = links.find(_.rel == Some(rel))

  def linksByType(mt: MediaType): IndexedSeq[Link] = links.filter(_.mediaType == Some(mt))

  def authorByName(name: String): Option[Person] = authors.find(_.name == name)

  def contributorByName(name: String): Option[Person] = contributors.find(_.name == name)

  def categoriesByScheme(scheme: String): IndexedSeq[Category] = categories.filter(_.scheme == Some(scheme))

  def categoryBySchemeAndTerm(scheme: String, term: String): Option[Category] = categories.find(x => x.scheme == Some(scheme) && x.term == term)

}

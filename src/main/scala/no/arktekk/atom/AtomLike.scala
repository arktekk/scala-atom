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

import extension.Extensible
import java.net.URI
import org.joda.time.DateTime

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
private[atom] trait AtomLike[T <: Extensible[_]] extends Extensible[T] { self: T =>
  def id: URI

  def title: TextConstruct[_]

  def rights: Option[TextConstruct[_]]

  def updated: DateTime

  def authors: Seq[Person]

  def contributors: Seq[Person]

  def categories: Seq[Category]

  def links: Seq[Link]

  def withId(id: URI): T

  def withTitle(title: TextConstruct[_]): T

  def withRights(rights: TextConstruct[_]):T

  def withUpdated(updated: DateTime):T

  def addAuthor(author: Person):T

  def addContributor(contrib: Person):T

  def addCategory(category: Category):T

  def addLink(link: Link):T = addLinks(Seq(link))

  def addLinks(links: Seq[Link]):T

  def linkByRel(rel: String) = links.find(_.rel == Some(rel))

  def selfLink : Option[Link] = linkByRel("self")

  def linksByType(mt: MediaType) = links.filter(_.mediaType == Some(mt))

  def authorByName(name: String) = authors.find(_.name == name)

  def contributorByName(name: String) = contributors.find(_.name == name)

  def categoriesByScheme(scheme: String) = categories.filter(_.scheme == Some(scheme))

  def categoryBySchemeAndTerm(scheme: String, term: String) = categories.find(x => x.scheme == Some(scheme) && x.term == term)

}

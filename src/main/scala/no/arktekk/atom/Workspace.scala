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

import com.codecommit.antixml._


/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
case class Workspace(wrapped: Elem) extends ElementWrapper {
  require(Elem.validateNamespace(wrapped, Atom.atompubNamespace), "Wrong namespace defined")
  require(wrapped.name == "workspace", "Wrong name of element")

  def title: Option[TextConstruct] = (wrapped \ atomSelector("title")).headOption.flatMap(TextConstruct(_))

  def collections: IndexedSeq[Collection] = (wrapped \ atomPubSelector("collection")).map(Collection(_))

  def withTitle(text: TextConstruct) = {
    replaceChildren(atomSelector("title"), IndexedSeq(ElementWrapper(text.toXML("title", Some("atom")))))
  }

  def addCollection(collection: Collection) = addChild(collection)

  def withCollections(collections: IndexedSeq[Collection]) = replaceChildren(atomPubSelector("collection"), collections)

  type T = Workspace

  protected def self = this

  def copy(elem: Elem) = new Workspace(elem)

  def findCollection(title: String): Option[Collection] = collections.find(_.title.filter(_.toString == title).isDefined)
}

object Workspace {
  def apply(): Workspace = apply(Elem(NamespaceBinding("app", Atom.atompubNamespace), "workspace"))

  def apply(title: TextConstruct): Workspace = apply(title, IndexedSeq.empty)

  def apply(title: TextConstruct, collections: IndexedSeq[Collection]): Workspace = {
    apply(Elem(
      NamespaceBinding("app", Atom.atompubNamespace),
      "workspace",
      Attributes(),
      Group.fromSeq(collections.map(_.wrapped))
    ))
  }
}

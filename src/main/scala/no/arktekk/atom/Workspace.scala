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
import Atom._


/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
case class Workspace(wrapped: Elem) extends ElementWrapper {
  def title: Option[TextConstruct] = (wrapped \ namespaceSelector(Atom.namespace, "title")).headOption.flatMap(TextConstruct(_))

  def collections: Seq[Collection] = (wrapped \ namespaceSelector(Atom.atompubNamespace, "collection")).map(Collection(_))

  def withTitle(text: TextConstruct) = {
    replaceChildren(namespaceSelector(Atom.namespace, "title"), Seq(new BasicElementWrapper(text.toXML("title", Some("atom")))))
  }

  def addCollection(collection: Collection) = addChild(collection)

  def withCollections(collections: Seq[Collection]) = replaceChildren(namespaceSelector(Atom.atompubNamespace, "collection"), collections)

  type T = Workspace

  protected def self = this

  def copy(elem: Elem) = new Workspace(elem)
}

object Workspace {
  def apply(): Workspace = apply(BasicElementWrapper.withName(NamespacedName(Atom.atompubNamespace, "app", "workspace")).wrapped)

  def apply(title: TextConstruct): Workspace = apply().withTitle(title)
}

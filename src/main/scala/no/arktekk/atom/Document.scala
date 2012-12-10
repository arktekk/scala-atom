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

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
case class Document[A <: DocumentLike] private[atom] (root: A, namespaces: Map[String, String]) {
  def defaultNamespace: String = namespaces.get("").getOrElse(Atom.namespace)
  def addNamespace(prefix: String, ns: String): Document[A] = copy(namespaces = namespaces.updated(prefix, ns))
  def withDefaultNamespace(ns: String): Document[A] = copy(namespaces = namespaces.updated("", ns))
}

object Document {
  def apply(feed: Feed): Document[Feed] = Document(feed, Map("" -> Atom.namespace))
  def apply(entry: Entry): Document[Entry] = Document(entry, Map("" -> Atom.namespace))
  def apply(c: Categories): Document[Categories] = Document(c, Map("" -> Atom.atompubNamespace, "atom" -> Atom.namespace))
  def apply(s: Service): Document[Service] = Document(s, Map("" -> Atom.atompubNamespace, "atom" -> Atom.namespace))
}

private[atom] trait DocumentLike
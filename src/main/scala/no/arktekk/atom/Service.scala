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
case class Service(wrapped: Elem) extends ElementWrapper {
  require(wrapped.name == "service" && Elem.validateNamespace(wrapped, Atom.atompubNamespace), "Wrong name or namespace of wrapped element")
  def workspaces : IndexedSeq[Workspace] = (wrapped \ atomPubSelector("workspace")).map(Workspace(_))

  def addWorkspace(workspace: Workspace): Service = addChild(workspace)

  def withWorkspaces(workspaces: IndexedSeq[Workspace]): Service = replaceChildren(NSRepr(Atom.atompubNamespace) -> "workspace", workspaces)

  type T = Service

  protected def self = this

  def copy(elem: Elem) = new Service(elem)

  def findWorkspace(title: String): Option[Workspace] = workspaces.find(_.title.filter(_.toString == title).isDefined)
}

object Service {
  def apply(): Service = apply(Elem(NamespaceBinding("app", Atom.atompubNamespace), "service"))

  def apply(workspaces: IndexedSeq[Workspace]): Service = {
    Service(Elem(
      NamespaceBinding("app", Atom.atompubNamespace),
      "service",
      Attributes(),
      Group.fromSeq(workspaces.map(_.wrapped))
    ))
  }
}

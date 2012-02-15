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
case class Service(wrapped: Elem) extends ElementWrapper {
  require(wrapped.name == "service" && wrapped.scope.find{case (_,y) => y == Atom.atompubNamespace}.isDefined, "Wrong name or namespace of wrapped element")
  def workspaces : Seq[Workspace] = (wrapped \ namespaceSelector(Atom.atompubNamespace, "workspace")).map(Workspace(_))

  def addWorkspace(workspace: Workspace) = addChild(workspace)

  def withWorkspaces(workspaces: Seq[Workspace]) = replaceChildren(namespaceSelector(Atom.atompubNamespace, "workspace"), workspaces)

  type T = Service

  protected def self = this

  def copy(elem: Elem) = new Service(elem)
}

object Service {
  def apply(): Service = apply(BasicElementWrapper.withName(NamespacedName(Atom.atompubNamespace, "app", "service")).wrapped)
}

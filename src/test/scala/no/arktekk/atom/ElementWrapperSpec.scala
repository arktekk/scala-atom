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

import org.specs2.mutable.Specification
import com.codecommit.antixml.{NamespaceBinding, PrefixedNamespaceBinding, QName}

/**
 * @author Erlend Hamnaberg<erlend.hamnaberg@arktekk.no>
 */

class ElementWrapperSpec extends Specification {
  "an element wrapper" should {
    "register namespace correctly" in {
      val elem = ElementWrapper.withName(NamespaceBinding("hello", "hello"), "hello")
      val addedNamespace = elem.addNamespace(None, Atom.namespace)
      addedNamespace.wrapped.namespaces.toList should beEqualTo(NamespaceBinding(Atom.namespace, NamespaceBinding("hello", "hello")).toList)
    }
    "generate namespace correctly" in {
      val elem = ElementWrapper.withName(NamespaceBinding("hello"), "hello")
      val addedNamespace = elem.addNamespace(Some(""), Atom.namespace)
      addedNamespace.wrapped.namespaces.toList should beEqualTo(NamespaceBinding("ns1", Atom.namespace, NamespaceBinding("hello")).toList)
    }
  }
}

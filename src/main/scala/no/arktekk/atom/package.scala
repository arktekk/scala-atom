/*
 * Copyright 2011 Arktekk AS
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

package no.arktekk

import java.net.URI
import com.codecommit.antixml.{Node, Elem, Selector}

package object atom {
  implicit def string2URI(iri: String) : URI = URI.create(iri)
  implicit def string2TextConstruct(text: String) : TextConstruct = TextConstruct.Textual(text)

  def namespaceSelector(namespace: String, element: String): Selector[Elem] = new Selector[Elem] {
    def apply(node: Node) = node match {
      case e@Elem(_, _, _, _, _) => e
      case _ => sys.error("woot?!")
    }

    def isDefinedAt(node: Node) = node match {
      case e@Elem(prefix, name, _, scopes, _) =>
        val x = scopes.find({
          case (_, `namespace`) => true;
          case _ => false
        }).map(_._1)
        prefix.equals(x)
      case _ =>
        false
    }
  }
}

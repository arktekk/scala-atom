package no.arktekk.atom

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

import java.net.URI
import collection.immutable.Map
import org.joda.time.format.DateTimeFormat
import io.Source
import com.codecommit.antixml._

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
object Atom {
  val namespace = "http://www.w3.org/2005/Atom"
  val namespaces: Map[String, String] = Map(("", namespace))
  val dateTimeFormat = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss.SS'Z'").withZoneUTC()

  private[atom] def onlyElementName(name: String) = {
    Elem(None, name, Attributes(), namespaces, Group.empty)
  }

  private[atom] def simple(name: String, value: String, attr: Attributes = Attributes()) = {
    withChildren(name, attr, Seq(Text(value)))
  }

  private[atom] def withChildren(name: String, attr: Attributes = Attributes(), children: Seq[Node]) = {
    Elem(None, name, attr, namespaces, Group.empty ++ children)
  }

  def parse[A <: Base](src: Source): A = {
    val elem = XML.fromSource(src)
    elem match {
      case e@Elem(_, "feed", _, _, _) if (e.scope.find {
        case (_, ns) => ns == namespace
      }.isDefined) => Feed(e).asInstanceOf[A]
      case e@Elem(_, "entry", _, _, _) if (e.scope.find {
        case (_, ns) => ns == namespace
      }.isDefined) => Entry(e).asInstanceOf[A]
      case e => throw new IllegalArgumentException("unknown XML here: %s".format(e))
    }
  }

  implicit def string2URI(iri: String): URI = URI.create(iri)

  implicit def string2TextConstruct(text: String): TextConstruct = TextConstruct.Textual(text)

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































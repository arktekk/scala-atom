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
package no.arktekk.atom

import java.net.URI
import com.codecommit.antixml.QName._
import com.codecommit.antixml.{QName, Elem, Attributes}

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
sealed trait Content {
  private[atom] def toXML(name: String): Elem
}

object Content {

  def apply(elem: Elem): Option[Content] = {
    val mediaType = elem.attrs.get("type").flatMap(MediaType(_))
    mediaType match {
      case mt@Some(_) if (elem.attrs.contains("href")) => Some(External(URI.create(elem.attrs("href")), mt))
      case Some(mt) => Some(Inline(mt, elem.children.head.asInstanceOf[Elem]))
      case None => TextConstruct(elem).map(Text(_))
    }
  }

  case class Text(text: TextConstruct) extends Content {
    private[atom] def toXML(name: String) = withChildren(name, Attributes((QName(None, "type"), text.textType.value)), Seq(text.value))
  }

  case class Inline(mediaType: MediaType, elem: Elem) extends Content {
    private[atom] def toXML(name: String) = withChildren(name, Attributes((QName(None, "type"), mediaType.toString)), Seq(elem))
  }

  case class External(href: URI, mediaType: Option[MediaType]) extends Content {
    private[atom] def toXML(name: String) = onlyElementName(name).copy(attrs = Attributes(("href" -> href.toString)) ++ mediaType.foldLeft(Map[QName, String]())((acc, mt) => acc + ("type" -> mt.toString)))
  }

}

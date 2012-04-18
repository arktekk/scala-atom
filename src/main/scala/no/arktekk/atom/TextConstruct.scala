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

import com.codecommit.antixml.Group._
import com.codecommit.antixml._
import com.codecommit.antixml.QName._
import com.codecommit.antixml.Selector._

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
sealed trait TextConstruct {
  def textType: TextType

  def value: Node

  def toString: String

  def toXML(name: String, prefix: Option[String] = None) = withChildren(name, Attributes(("type", textType.value)), Group(value), prefix)
}

object TextConstruct {
  def apply(elem: Elem): Option[TextConstruct] = {
    val textType = elem.attrs.get("type")
    textType.flatMap(TextType(_)).map{
      case TextType.HTML => HTML((elem \ text).head)
      case TextType.XHTML => XHTML(Div((elem \ "div").head))
      case TextType.TEXT => Textual((elem \ text).head)
    }.orElse(Some(Textual((elem \ text).head)))
  }

  def unapply(construct: TextConstruct) = Some((construct.textType, construct.value))

  case class Textual(text: String) extends TextConstruct {
    val textType = TextType.TEXT

    override def toString = text

    def value = Text(text)
  }

  case class XHTML(div: Div) extends TextConstruct {
    def textType = TextType.XHTML

    lazy val value = div.toXML

    override def toString = value.toString()
  }

  case class HTML(html: String) extends TextConstruct {
    def textType = TextType.HTML

    lazy val value = CDATA(html)

    override def toString = html
  }

  case class Div private[atom](elem: Elem) {
    require("div" == elem.name)

    def toXML: Elem = elem
  }

  object Div {
    val namespace = "http://www.w3.org/1999/xhtml"

    def apply(text: String): Div = Div(XML.fromString("<div xmlns=\"%s\">%s</div>".format(namespace, text)))
  }

}

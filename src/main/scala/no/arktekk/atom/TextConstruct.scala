package no.arktekk.atom

import com.codecommit.antixml.Group._
import com.codecommit.antixml._
import com.codecommit.antixml.QName._
import com.codecommit.antixml.Selector._

/**
 * Created by IntelliJ IDEA.
 * User: maedhros
 * Date: 12/22/11
 * Time: 8:12 PM
 * To change this template use File | Settings | File Templates.
 */

sealed trait TextConstruct {
  def textType: TextType

  def value: Node

  def toXML(name: String) = Atom.withChildren(name, Attributes(("type", textType.value)), Group(value))
}

object TextConstruct {
  def apply(elem: Elem): Option[TextConstruct] = {
    val textType = elem.attrs.get("type")
    textType.flatMap(TextType(_)).map {
      case TextType.HTML => HTML((elem \ text).head)
      case TextType.XHTML => XHTML(Div((elem \ "div").head))
      case TextType.TEXT => Textual((elem \ text).head)
    }
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
  }

  case class HTML(html: String) extends TextConstruct {
    def textType = TextType.HTML

    lazy val value = CDATA(html)
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

sealed trait TextType {
  def value: String
}

object TextType {
  def apply(name: String): Option[TextType] = name match {
    case HTML.value => Some(HTML)
    case XHTML.value => Some(XHTML)
    case TEXT.value => Some(TEXT)
    case _ => None
  }

  case object HTML extends TextType {
    val value = "html"
  }

  case object XHTML extends TextType {
    val value = "xhtml"
  }

  case object TEXT extends TextType {
    val value = "text"
  }
}

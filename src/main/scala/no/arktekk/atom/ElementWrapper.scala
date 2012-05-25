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

import com.codecommit.antixml._
import extension.AtomExtension
import java.io._

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
trait ElementWrapper {
  type T <: ElementWrapper

  protected def self : T

  def wrapped: Elem

  def copy(elem: Elem): T

  def addChild(text: String):T = addChild(Text(text))

  def addChild(name: String, text: String):T = {
    val ns = wrapped.scope.get(wrapped.prefix.getOrElse("")).getOrElse("")
    addChild(NamespacedName(ns, QName(wrapped.prefix, name)), text)
  }

  def addChild(name: NamespacedName, text: String):T = addChild(name, Text(text))

  def addChild(name: NamespacedName, node: Node):T = addChild(ElementWrapper.withNameAndChildren(name, Group(node)))

  def addChild(w: ElementWrapper):T = addChild(w.wrapped)

  def addChild(node: Node):T = copy(wrapped.copy(children = wrapped.children ++ Group(node)))

  def apply[A >: T, B](ext: AtomExtension[A, B], value: B) : T = {
    val applied = updateAttributes(ext.toAttributes(value))
    addChildren(ext.toChildren(value, applied))
  }

  def extract[A >: T, B](ext: AtomExtension[A, B]): B = ext.fromLike(self)

  private def updateAttributes(attrs: Seq[NamespacedAttribute]): T = {
    val fold = attrs.foldLeft((Map[String, String](), Map[QName, String]())){
      case ((x,y),z) => (x ++ z.ns.toMap) -> (y + (z.ns.qName -> z.value))
    }
    copy(wrapped.copy(scope = wrapped.scope ++ fold._1, attrs = wrapped.attrs ++ fold._2))
  }

  def addChildren(children: Seq[ElementWrapper]) : T = {
    addChildren(Group.fromSeq(children.map(_.wrapped)))
  }

  def addChildren(children: Group[Node]) : T = {
    if (children.isEmpty) self
    else copy(wrapped.copy(children = wrapped.children ++ children))
  }

  def replaceChildren(selector: Selector[Elem], children: Seq[ElementWrapper]) : T = {
    replaceChildren(selector, Group.fromSeq(children.map(_.wrapped)))
  }

  def replaceChildren(selector: Selector[Elem], children: Group[Node]) : T = {
    if (children.isEmpty) self
    else copy(removeChildren(selector).copy(children = wrapped.children ++ children))
  }

  def withChildren(children: Seq[ElementWrapper]) : T = {
    withChildren(Group.fromSeq(children.map(_.wrapped)))
  }

  def withChildren(children: Group[Node]) : T = {
    if (children.isEmpty) self
    else copy(wrapped.copy(children = children))
  }

  def withAttribute(name: String, value: Any): T = withAttribute(QName(None, name), value)

  def withAttribute(name: QName, value: Any): T = copy(wrapped.copy(attrs = wrapped.attrs + (name -> value.toString)))

  protected def removeChildren(selector: Selector[Elem]): Elem = {
    val zipper = (wrapped \ selector).take(0)
    zipper.unselect.headOption.map(_.asInstanceOf[Elem]).getOrElse(wrapped)
  }

  def addNamespaces(namespaces: Map[String, String]): T  = {
    if (namespaces.isEmpty) self
    else {
      def nextValidPrefix = {
        var i = 1
        while (wrapped.scope.contains("ns" + i)) {
          i = i + 1
        }
        "ns" + i
      }
      def mapit(namespaces: Map[String, String], tuple: (String, String)) = tuple match {
        case (x, y) if (namespaces.find{case (_, z) => z == y}.isDefined) => namespaces
        case ("", y) if (namespaces.get("").isEmpty) => { //if the empty namespace has not been defined already
          namespaces + ("" -> y)
        }
        case ("", y) => {
          val p = nextValidPrefix
          namespaces + (p -> y)
        }
        case (x, y) => namespaces + (x -> y)
      }
      val currentNS = namespaces.foldLeft(wrapped.scope){case (map, t) => mapit(map, t)}

      if (currentNS == wrapped.scope) self else copy(wrapped.copy(scope = currentNS))
    }
  }

  def addNamespace(prefix: Option[String], namespace: String): T  = addNamespace((prefix.getOrElse("").trim(), namespace.trim()))

  def addNamespace(prefixNS: (String, String)): T = {
    addNamespaces(Map(prefixNS))
  }

  /**
   * Serializes the ElementWrapper to the given writer. Note that the writer will be flushed. but not closed,
   * @param writer the writer to use
   * @param charset requires a charset to be used.
   */
  def writeTo(writer: Writer)(implicit charset: Charset) {
    XMLSerializer(charset.name, true).serializeDocument(wrapped, writer)
  }

  /**
   * @see #writeTo(Writer)
   */
  def writeTo(stream: OutputStream)(implicit charset: Charset) {
    writeTo(new OutputStreamWriter(stream, charset.wrapped))(charset)
  }

  /**
   * @see #writeTo(Writer)
   */
  def writeTo(file: File)(implicit charset: Charset) {
    val writer = new FileWriter(file)
    try {
      writeTo(writer)(charset)
    }
    finally {
      writer.close()
    }
  }
}

object ElementWrapper {
  def apply(elem: Elem): ElementWrapper = new BasicElementWrapper(elem)

  def withName(name: NamespacedName): ElementWrapper = {
    withNameAndAttributes(name)
  }

  def withNameAndAttributes(name: NamespacedName, attrs: Attributes = Attributes()): ElementWrapper = {
    apply(Elem(name.prefix, name.name, attrs, name.toMap, Group.empty))
  }

  def withNameAndText(name: NamespacedName, text: String): ElementWrapper = {
    new BasicElementWrapper(Elem(name.prefix, name.name, Attributes(), name.toMap, Group(Text(text))))
  }

  def withNameAndChildren(name: NamespacedName, children: Group[Node]): ElementWrapper = {
    new BasicElementWrapper(Elem(name.prefix, name.name, Attributes(), name.toMap, children))
  }
}

case class BasicElementWrapper(elem: Elem) extends ElementWrapper {
  type T = ElementWrapper

  protected val self = this

  def wrapped = elem

  def copy(elem: Elem) = new BasicElementWrapper(elem)
}

object BasicElementWrapper {
  def withName(name: NamespacedName): BasicElementWrapper = {
    apply(Elem(name.prefix, name.name, Attributes(), name.toMap, Group.empty))
  }

  def withNameAndAttributes(name: NamespacedName, attrs: Attributes = Attributes()): BasicElementWrapper = {
    apply(Elem(name.prefix, name.name, attrs, name.toMap, Group.empty))
  }
}

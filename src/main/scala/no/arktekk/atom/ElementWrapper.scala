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
  
  def addChild(w: ElementWrapper) = copy(wrapped.copy(children = wrapped.children ++ List(w.wrapped)))
  
  def addChild(text: String) = copy(wrapped.copy(children = wrapped.children ++ List(Text(text))))
  
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
    if (children.isEmpty) self
    else copy(wrapped.copy(children = wrapped.children ++ children.map(_.wrapped)))
  }

  def replaceChildren(selector: Selector[Elem], children: Seq[ElementWrapper]) : T = {
    if (children.isEmpty) self
    else copy(removeChildren(selector).copy(children = wrapped.children ++ children.map(_.wrapped)))
  }

  def withChildren(children: Seq[ElementWrapper]) : T = {
    if (children.isEmpty) self
    else copy(wrapped.copy(children = children.map(_.wrapped)))
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

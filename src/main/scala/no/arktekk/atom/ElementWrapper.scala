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

  def \[B](selector: Selector[B]) = wrapped \ selector

  def copy(elem: Elem): T

  def addChild(text: String):T = addChild(Text(text))

  def addChild(name: String, text: String):T = {
    val binding = wrapped.namespaces
    addChild(binding, name, text)
  }

  def addChild(ns: NamespaceBinding, name: String, text: String):T = addChild(ns, name, Text(text))

  def addChild(ns: NamespaceBinding, name: String, node: Node):T = addChild(ElementWrapper.withNameAndChildren(ns, name, Group(node)))

  def addChild(w: ElementWrapper):T = addChild(w.wrapped)

  def addChild(node: Node):T = copy(wrapped.copy(children = wrapped.children ++ Group(node)))

  def apply[A >: T, B](ext: AtomExtension[A, B], value: B) : T = {
    val applied = updateAttributes(ext.toAttributes(value))
    addChildren(ext.toChildren(value, applied))
  }

  def extract[A >: T, B](ext: AtomExtension[A, B]): B = ext.fromLike(self)

  private def updateAttributes(attrs: Seq[(QName, String)]): T = {
    copy(wrapped.withAttributes(Attributes(attrs : _*)))
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

  def attr(name: QName) = wrapped.attrs.get(name)

  def withAttribute(name: String, value: Any): T = withAttribute(QName(name), value)

  def withAttribute(name: QName, value: Any): T = copy(wrapped.withAttribute(name, value.toString))

  protected def removeChildren(selector: Selector[Elem]): Elem = {
    val zipper = (wrapped \ selector).take(0)
    zipper.unselect.headOption.map(_.asInstanceOf[Elem]).getOrElse(wrapped)
  }

  def addNamespaces(namespaces: Map[String, String]): T  = {
    if (namespaces.isEmpty) self
    else {
      val copied = wrapped.addNamespaces(namespaces)
      if (copied eq wrapped) self else copy(copied)
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

  def withName(name: String): ElementWrapper = {
    withName(NamespaceBinding.empty, name)
  }

  def withName(ns: NamespaceBinding, name: String): ElementWrapper = {
    withNameAndAttributes(ns, name)
  }

  def withNameAndAttributes(ns: NamespaceBinding, name: String, attrs: Attributes = Attributes()): ElementWrapper = {
    apply(Elem(ns, name, attrs))
  }

  def withNameAndText(ns: NamespaceBinding, name: String, text: String): ElementWrapper = {
    new BasicElementWrapper(Elem(ns, name, Attributes(), Group(Text(text))))
  }

  def withNameAndChildren(ns: NamespaceBinding, name: String, children: Group[Node]): ElementWrapper = {
    new BasicElementWrapper(Elem(ns, name, Attributes(), children))
  }
}

case class BasicElementWrapper(wrapped: Elem) extends ElementWrapper {
  type T = ElementWrapper

  protected val self = this

  def copy(elem: Elem) = new BasicElementWrapper(elem)
}

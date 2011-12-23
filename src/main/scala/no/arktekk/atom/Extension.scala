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

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
trait Extension {
  def namespaced: Namespaced
  def attributes: Attributes
  def children: List[Extension]

  def toXML: Elem = {
    Elem(namespaced.prefix, namespaced.name, attributes, namespaced.asMap, Group.empty ++ children.map(_.toXML))
  }
}

case class Namespaced(namespace: Option[String], prefix: Option[String], name: String) {
  namespace match {
    case None if(prefix.isDefined) =>  Console.err.println("No namespace was defined for %s, but prefix defined. Will attempt using parent namespace".format(name))
    case _ =>
  }
  def asMap = {
    namespace.foldLeft(Map[String, String]())((map, n) => map + (prefix.getOrElse("") -> n))
  }
}

object Namespaced {
  def apply(namespace:String, prefix: String, name:String): Namespaced = apply(Some(namespace), Some(prefix), name)
  def apply(namespace:String, qname: QName): Namespaced = apply(Some(namespace), qname.prefix, qname.name)
  def apply(name: String): Namespaced = apply(None, None, name)
}

abstract class AbstractExtension(val namespaced: Namespaced, val attributes: Attributes = Attributes(), val children: List[Extension] = Nil) extends Extension

case class TextElementExtension(namespaced: Namespaced, attributes: Attributes = Attributes(), text: Option[String]) extends Extension {

  override final def toXML = {
    super.toXML.copy(children = Group.empty ++ text.map(Text(_)))
  }

  def children = Nil
}

object TextElementExtension {
  def apply(namespaced: Namespaced, text: String): TextElementExtension = apply(namespaced, Attributes(), Some(text))
}

case class SimpleExtension(namespaced: Namespaced, attributes: Attributes = Attributes(), children: List[Extension] = Nil) extends Extension

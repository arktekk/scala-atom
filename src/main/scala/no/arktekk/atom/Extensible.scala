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

import com.codecommit.antixml.Elem

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
trait Extensible {
  type A <: Extensible

  def wrapped: Elem

  def getExtension[A <: ElementWrapper](ns: Namespaced)(implicit manifest: Manifest[A]): Option[A] = {
    getExtensions(ns).headOption
  }

  def getExtensions[A <: ElementWrapper](ns: Namespaced)(implicit manifest: Manifest[A]): List[A] = {
    val selected = (wrapped \ namespaceSelector(ns.namespace, ns.name))
    val ctor = Option(manifest.erasure.getConstructor(classOf[Elem]))
    ctor.foreach(_.setAccessible(true))
    selected.flatMap {
      case s: Elem => ctor.map(_.newInstance(s).asInstanceOf[A])
    }.toList
  }

  def copy(wrapped: Elem): A

  def addExtension(wrapper: ElementWrapper): A = {
    copy(wrapped.copy(children = wrapped.children :+ wrapper.wrapped))
  }
}

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
import extension.{AtomExtension}

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
trait ElementWrapper {
  type T <: ElementWrapper

  protected def self : T

  def wrapped: Elem

  def copy(elem: Elem): T
  
  def addChild(w: ElementWrapper) = copy(wrapped.copy(children = wrapped.children ++ List(w.wrapped)))
  
  def apply[A >: T, B](ext: AtomExtension[A, B], value: B) : T = {
    val applied = updateAttributes(ext.toAttributes(value))
    addChildren(ext.toChildren(value, applied))
  }
  private def updateAttributes(attrs: Seq[NamespacedAttribute]): T = {
    val fold = attrs.foldLeft((Map[String, String](), Map[QName, String]())){
      case ((x,y),z) => (x ++ z.ns.toMap) -> (y + (z.ns.toQName -> z.value))
    }
    copy(wrapped.copy(scope = wrapped.scope ++ fold._1, attrs = wrapped.attrs ++ fold._2))
  }

  def addChildren[B](children: Seq[ElementWrapper]) : T = {
    if (children.isEmpty) self
    else copy(wrapped.copy(children = wrapped.children ++ children.map(_.wrapped)))
  }
}

object ElementWrapper {
  def apply(elem: Elem): ElementWrapper = new BasicElementWrapper(elem)
  
  class BasicElementWrapper(elem: Elem) extends ElementWrapper {
    type T = ElementWrapper

    protected val self = this

    def wrapped = elem

    def copy(elem: Elem) = new BasicElementWrapper(elem)
  }
}

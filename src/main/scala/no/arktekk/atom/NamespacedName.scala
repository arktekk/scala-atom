/*
 * Copyright 2012 Arktekk AS
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

import com.codecommit.antixml.QName

final class NamespacedName private(val namespace: Option[String], val prefix: Option[String], val name: String) {
  require(name != null, "Null name not allowed")

  lazy val qName = QName(prefix, name)

  def toMap = {
    if (!namespace.isDefined) Map[String, String]() else Map((prefix.getOrElse("") -> namespace.get))
  }

  override def equals(obj: Any) = obj match {
    case NamespacedName(ns, p, n) => ns == namespace && p == prefix && name == name
    case _ => false
  }

  override def hashCode() = 31 * (namespace.hashCode() + prefix.hashCode() + name.hashCode)
}

object NamespacedName {
  private def apply(ns: Option[String], prefix: Option[String], name: String): NamespacedName = {
    new NamespacedName(ns.filterNot(_.trim.isEmpty), prefix, name)
  }

  def apply(namespace: String, prefix: String, name: String): NamespacedName = apply(Some(namespace), Some(prefix), name)

  def apply(namespace: String, qname: QName): NamespacedName = apply(Some(namespace), qname.prefix, qname.name)

  def apply(namespace: String, name: String): NamespacedName = apply(Some(namespace), None, name)
  
  def apply(name: String): NamespacedName = apply(None, None, name)

  implicit def toNamespacedName(name: String): NamespacedName = apply(name)

  def unapply(nn: NamespacedName) = Some((nn.namespace, nn.prefix, nn.name))
}

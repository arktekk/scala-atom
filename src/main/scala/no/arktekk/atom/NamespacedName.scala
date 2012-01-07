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

/**
 * Created by IntelliJ IDEA.
 * User: maedhros
 * Date: 1/2/12
 * Time: 12:15 PM
 * To change this template use File | Settings | File Templates.
 */

case class NamespacedName private(namespace: Option[String], prefix: Option[String], name: String) {
  require(namespace != null, "Null namespace not allowed")
  require(name != null, "Null name not allowed")

  lazy val qName = QName(prefix, name)

  def toMap = {
    if (!namespace.isDefined) Map[String, String]() else Map((prefix.getOrElse("") -> namespace.get))
  }
}

object NamespacedName {
  def apply(namespace: String, prefix: String, name: String): NamespacedName = apply(Some(namespace), Some(prefix), name)

  def apply(namespace: String, qname: QName): NamespacedName = apply(Some(namespace), qname.prefix, qname.name)

  def apply(namespace: String, name: String): NamespacedName = apply(Some(namespace), None, name)
  
  def apply(name: String): NamespacedName = apply(None, None, name)
}

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

/**
 * @author Erlend Hamnaberg<erlend.hamnaberg@arktekk.no>
 */

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

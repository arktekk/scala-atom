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

package no.arktekk.atom.extension.thr

import com.codecommit.antixml.Elem
import no.arktekk.atom.extension.OptionSelectableElementWrapperAtomExtension
import java.net.URI
import no.arktekk.atom._

/**
 * @author Erlend Hamnaberg<erlend.hamnaberg@arktekk.no>
 */

object InReplyToAtomExtension extends OptionSelectableElementWrapperAtomExtension[Entry, InReplyTo] {
  protected def selector = namespaceSelector(ThreadingConstants.ns, "in-reply-to")

  protected def function = (e) => InReplyTo(e)

  def toChildren(a: Option[InReplyTo], parent: ElementWrapper) = a.toSeq
}

case class InReplyTo(wrapped: Elem) extends ElementWrapper {
  type T = InReplyTo

  protected def self = this

  def copy(elem: Elem) = new InReplyTo(elem)

  def ref = wrapped.attrs.get("ref").map(URI.create(_)).get

  def href = wrapped.attrs.get("href").map(URI.create(_))

  def source = wrapped.attrs.get("source").map(URI.create(_))

  def mediaType = wrapped.attrs.get("type").flatMap(MediaType(_))

}

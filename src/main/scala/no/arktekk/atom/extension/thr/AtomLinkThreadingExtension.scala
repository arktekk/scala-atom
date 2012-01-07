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

import org.joda.time.DateTime
import no.arktekk.atom._
import extension.AtomExtension
import no.arktekk.atom.Atom._

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
object AtomLinkThreadingExtension extends AtomExtension[Link, AtomThreading] {
  val count = NamespacedName(ThreadingConstants.ns, ThreadingConstants.prefix, "count")
  val updated = NamespacedName(ThreadingConstants.ns, ThreadingConstants.prefix, "updated")

  def fromLike(like: Link) = {
    val c = like.wrapped.attrs.get(count.qName).map(_.toInt)
    val u = like.wrapped.attrs.get(updated.qName).map(parseDateTime(_))
    AtomThreading(c, u)
  }


  override def toAttributes(a: AtomThreading) = {
    a.count.map(x => NamespacedAttribute(count, x.toString)).toSeq ++
      a.updated.map(x => NamespacedAttribute(updated, dateTimeToString(x))).toSeq
  }

  def toChildren(a: AtomThreading, wrapper: ElementWrapper) = Nil
}

case class AtomThreading(count: Option[Int] = None, updated: Option[DateTime] = None)


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

import java.net.URI

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
sealed trait FeedLike[T <: AtomLike[_]] extends AtomLike[T] { self: T =>
  def subtitle: Option[TextConstruct[_]]

  def entries: IndexedSeq[EntryLike]

  def logo: Option[URI]

  def icon: Option[URI]

  def withSubtitle(title: TextConstruct[_]): T

  def withLogo(logo: URI): T

  def withIcon(icon: URI): T

  def addEntry(entry: EntryLike): T

  def addEntries(seq: IndexedSeq[EntryLike]): T

  def withEntries(seq: IndexedSeq[EntryLike]): T

  def entryById(id: URI) = entries.find(_.id == id)

  def entryBySelfLink(href: URI) = entries.find(_.selfLink.filter(_.href == href).isDefined)
}

trait Source extends FeedLike[Source]

trait Feed extends FeedLike[Feed]
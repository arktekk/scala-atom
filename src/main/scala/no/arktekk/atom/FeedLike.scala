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
trait FeedLike extends AtomLike[FeedLike] {
  def subtitle: Option[TextConstruct]

  def entries: IndexedSeq[EntryLike]

  def logo: Option[URI]

  def icon: Option[URI]

  def withSubtitle(title: TextConstruct): FeedLike

  def withLogo(logo: URI): FeedLike

  def withIcon(icon: URI): FeedLike

  def addEntry(entry: EntryLike): FeedLike

  def addEntries(seq: IndexedSeq[EntryLike]): FeedLike

  def withEntries(seq: IndexedSeq[EntryLike]): FeedLike

  def entryById(id: URI) = entries.find(_.id == id)

  def entryBySelfLink(href: URI) = entries.find(_.selfLink.filter(_.href == href).isDefined)
}

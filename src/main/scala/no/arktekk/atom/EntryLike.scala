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

import org.joda.time.DateTime
import no.arktekk.atom.Atom._
import com.codecommit.antixml._
import com.codecommit.antixml.Selector._

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
trait EntryLike extends AtomLike {
  def published: Option[DateTime] = (wrapped \ "published" \ text).headOption.map(dateTimeFormat.parseDateTime(_))

  def content: Option[Content] = (wrapped \ "content").headOption.flatMap(Content(_))

  def summary: Option[Content] = (wrapped \ "summary").headOption.flatMap(Content(_))

  def withPublished(published: DateTime) = copy(removeChild("published").copy(children = wrapped.children ++ List(simple("published", dateTimeFormat.print(published)))))

  def withSummary(summary: Content) = copy(removeChild("summary").copy(children = wrapped.children ++ List(summary.toXML("summary"))))

  def withContent(content: Content) = copy(removeChild("content").copy(children = wrapped.children ++ List(content.toXML("content"))))

  def removeContent() = copy(removeChild("content"))

  def removeSummary() = copy(removeChild("summary"))
}

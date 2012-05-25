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

import org.joda.time.DateTime

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
trait EntryLike extends AtomLike {
  def published: Option[DateTime] = elementText("published").headOption.map(parseDateTime(_))

  def content: Option[Content] = element("content").headOption.flatMap(Content(_))

  def summary: Option[Content] = element("summary").headOption.flatMap(Content(_))

  def withPublished(published: DateTime) = copy(removeChildren("published")).addChild("published", dateTimeToString(published))

  def withSummary(summary: Content) = removeSummary().addChild(summary.toXML("summary"))

  def withContent(content: Content) = removeContent().addChild(content.toXML("content"))

  def removeContent() = copy(removeChildren("content"))

  def removeSummary() = copy(removeChildren("summary"))
}

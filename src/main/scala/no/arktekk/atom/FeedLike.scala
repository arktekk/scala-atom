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
import com.codecommit.antixml._

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
trait FeedLike extends AtomLike {
  def subtitle: Option[TextConstruct] = (wrapped \ atomSelector("subtitle")).headOption.flatMap(TextConstruct(_))

  def entries: List[Entry] = (wrapped \ atomSelector("entry")).map(Entry(_)).toList

  def logo = (wrapped \ atomSelector("logo") \ text).headOption.map(URI.create(_))

  def icon = (wrapped \ atomSelector("icon") \ text).headOption.map(URI.create(_))

  def withSubtitle(title: TextConstruct) = copy(removeChildren("subtitle").copy(children = wrapped.children ++ List(title.toXML("subtitle"))))

  def withLogo(logo: URI) = copy(removeChildren("logo").copy(children = wrapped.children ++ List(simple("logo", logo.toString))))

  def withIcon(icon: URI) = copy(removeChildren("icon").copy(children = wrapped.children ++ List(simple("icon", icon.toString))))
}

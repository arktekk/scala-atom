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
trait FeedLike extends AtomLike {
  def subtitle: Option[TextConstruct] = element("subtitle").headOption.flatMap(TextConstruct(_))

  def logo: Option[URI] = elementText("logo").headOption.map(URI.create(_))

  def icon: Option[URI] = elementText("icon").headOption.map(URI.create(_))

  def generator: Option[Generator] = element("generator").headOption.map(Generator(_))

  def addEntry(entry: Entry): T = {
    copy(wrapped.copy(children = wrapped.children ++ List(entry.wrapped)))
  }

  def withSubtitle(title: TextConstruct): T = replaceChildren(atomSelector("subtitle"), title.toXML("subtitle").toGroup)

  def withLogo(logo: URI): T = replaceChildren(atomSelector("logo"), atomTextElem("logo", logo.toString).toGroup)

  def withIcon(icon: URI): T = replaceChildren(atomSelector("icon"), atomTextElem("icon", icon.toString).toGroup)

  def withGenerator(generator: Generator): T = replaceChildren(atomSelector("generator"), generator.wrapped.toGroup)
}

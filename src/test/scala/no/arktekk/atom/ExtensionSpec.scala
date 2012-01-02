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

import org.specs2.mutable.Specification
import io.Source
import com.codecommit.antixml._

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
class ExtensionSpec extends Specification {
  "A parser that handles extensions" should {
    "parse correctly" in {
      val entry : Entry = Atom.parse(Source.fromInputStream(getClass.getResourceAsStream("/entry-with-extension.xml")))
      entry must not be null
    }
    "find extension in parsed" in {
      val entry : Entry = Atom.parse(Source.fromInputStream(getClass.getResourceAsStream("/entry-with-extension.xml")))
      val simple = entry.getExtension[SimpleExtension](Namespaced("urn:ext:ext", "hello"))
      simple mustEqual Some(SimpleExtension((entry.wrapped \ "hello").head.asInstanceOf[Elem]))
      simple.get.value mustEqual "Hi!"
    }
  }
}

case class SimpleExtension(override val wrapped: Elem) extends ElementWrapper(wrapped) {
  def value = (wrapped \ text).head
}

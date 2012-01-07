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

import extension.{SimpleTextElementWrapper, AtomExtension}
import org.specs2.mutable.Specification
import io.{Source => IOSource}
import com.codecommit.antixml._
import java.net.URI
import org.joda.time.DateTime
import Atom._

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
class ExtensionSpec extends Specification {
  "A parser that handles extensions" should {
    "parse correctly" in {
      val entry : Entry = Atom.parse(IOSource.fromInputStream(getClass.getResourceAsStream("/entry-with-extension.xml"))).right.get
      entry must not be null
    }
    "find extension in parsed" in {
      val entry : Entry = Atom.parse(IOSource.fromInputStream(getClass.getResourceAsStream("/entry-with-extension.xml"))).right.get
      val simple = HelloExtension.fromLike(entry)
      simple.value mustEqual "Hi!"
    }

    "Add extension to entry" in {
      val entry = Entry(URI.create("hellothingy"), "Title", new DateTime())
      val updatedEntry = entry.apply(HelloExtension, Hello("Hi!"))
      entry must not beTheSameAs(updatedEntry)
      val simple = HelloExtension.fromLike(updatedEntry)
      simple.value mustEqual "Hi!"
    }
  }
}

case class Hello(value: String)

object HelloExtension extends AtomExtension[Entry, Hello] {
  def fromLike(like: Entry) = new Hello((like.wrapped \ namespaceSelector("urn:ext:ext", "hello") \ text).head)

  def toChildren(a: Hello, w: ElementWrapper) = Seq(SimpleTextElementWrapper(NamespacedName("urn:ext:ext", "ext", "hello"), a.value))
}

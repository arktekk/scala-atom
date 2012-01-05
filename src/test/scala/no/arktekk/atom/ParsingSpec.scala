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
import io.{Source => IOSource}
import java.net.URI

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
class ParsingSpec extends Specification {

  "A parser" should {
    "create a feed from feed.xml" in {
      val feed : Feed = Atom.parse(IOSource.fromInputStream(getClass.getResourceAsStream("/feed.xml"))).right.get
      feed.id mustEqual URI.create("urn:uuid:something-random")
      feed.updated mustEqual Atom.dateTimeFormat.parseDateTime("2011-01-01T08:00:00.00Z")
      feed.authors.length mustEqual 1
      feed.authors.head.name mustEqual "Example"
      feed.entries.length mustEqual 2
    }

    "create a feed from feed-with-non-default-namespace.xml" in {
      val feed : Feed = Atom.parse(IOSource.fromInputStream(getClass.getResourceAsStream("/feed-with-non-default-namespace.xml"))).right.get
      feed.id mustEqual URI.create("urn:uuid:something-random")
      feed.updated mustEqual Atom.dateTimeFormat.parseDateTime("2011-01-01T08:00:00.00Z")
      feed.authors.length mustEqual 1
      feed.authors.head.name mustEqual "Example"
      feed.entries.length mustEqual 1
    }

    "create an entry from entry.xml" in {
      val entry : Entry = Atom.parse(IOSource.fromInputStream(getClass.getResourceAsStream("/entry.xml"))).right.get
      entry.id mustEqual URI.create("urn:id:1")
      entry.updated mustEqual Atom.dateTimeFormat.parseDateTime("2011-01-01T08:00:00.00Z")
      entry.authors.length mustEqual 1
      val person: Person = entry.authors.head
      person.name mustEqual "Someone"
      person.email mustEqual Some("test@example.com")
      person.url mustEqual Some("http://www.example.org")
      entry.summary.get.asInstanceOf[Content.Text].text.toString mustEqual "summary"
    }

    "fail with parsing entry-without-namespace-def.xml" in {
      val ex = Atom.parse(IOSource.fromInputStream(getClass.getResourceAsStream("/entry-without-namespace-def.xml"))).left.get
      ex must not be null
      ex.getMessage must contain("unknown XML here")
    }
  }
}

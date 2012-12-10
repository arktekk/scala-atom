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

import org.specs2.mutable.Specification
import io.Source
import java.net.URI
import no.arktekk.atom._

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */

class ThreadingSpec extends Specification {
  "Atom threading extension" should {
    "extract attributes correctly" in {
      val entry = Atom.parseEntry(Source.fromInputStream(getClass.getResourceAsStream("/extensions/thr-count-updated.xml"))).right.get
      val link = entry.linkByRel("replies").get
      val thr = link.extract(AtomLinkThreadingExtension)
      thr should be equalTo(AtomThreading(Some(10), Some(parseDateTime("2005-07-28T12:10:00Z"))))
    }

    "extract inreply-to correctly" in {
      val entry = Atom.parseEntry(Source.fromInputStream(getClass.getResourceAsStream("/extensions/in-reply-to.xml"))).right.get
      val inReplyTo = entry.extract(InReplyToAtomExtension)
      inReplyTo.get.ref should be equalTo(URI.create("tag:example.org,2005:1"))
      inReplyTo.get.mediaType should be equalTo(MediaType("application/xhtml+xml"))
      inReplyTo.get.href should be equalTo(Some(URI.create("http://www.example.org/entries/1")))
    }
  }
}

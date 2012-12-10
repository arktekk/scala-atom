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

package no.arktekk.atom.extension.georss

import org.specs2.mutable.Specification
import java.net.URI
import org.joda.time.DateTime
import no.arktekk.atom.{ElementWrapper, Entry}
import com.codecommit.antixml.{text => TextSelector, NamespaceBinding, QName}

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
class PointSpec extends Specification {
  "A Point" should {
    "be deserialized correctly" in {
      val entry = Entry(URI.create("1"), "hei", new DateTime()).
        addChild(ElementWrapper.withNameAndText(NamespaceBinding("foo", GeorssConstants.ns), "point", "23.23 0"))
      val point = entry.extract(PointAtomExtension()).head
      point.toValue("##.####") must beEqualTo("23.23 0")
    }
    "be serialized correctly" in {
      val point = Point(23.234567, 76.123546)
      val serialized = PointAtomExtension().toChildren(Some(point)).head
      (serialized.wrapped \ TextSelector).head must beEqualTo(point.toValue("##.#####"))
    }
  }
}

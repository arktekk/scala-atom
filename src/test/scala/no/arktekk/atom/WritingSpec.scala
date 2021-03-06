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

import org.specs2.mutable.Specification
import java.io.File
import io.Codec


/**
 * @author Erlend Hamnaberg<erlend.hamnaberg@arktekk.no>
 */

class WritingSpec extends Specification {
  "An elementWrapper" should {
    "be able to write with a default encoding" in {
      val tempFile = File.createTempFile("foo", ".xml")
      ElementWrapper.withName("hello").writeTo(tempFile)
      tempFile should beAFile
      tempFile.length() should beGreaterThan(0L)
    }
  }
}

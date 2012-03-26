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

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
class MediaTypeSpec extends Specification {
  "a media type" should {
    "be parsed correctly" in {
      MediaType("application/xml") must be equalTo(Some(MediaType("application", "xml")))
    }
    "be parsed correctly with parameter" in {
      MediaType("application/xml; charset=ISO-8859-1") must be equalTo(Some(MediaType("application", "xml", Map("charset" -> "ISO-8859-1"))))
    }
    /*
     NOT supported by javax.activation
    "be parsed correctly with comment and parameter" in {
      MediaType("text/plain; charset=ISO-8859-1 (Plain text) ") must be equalTo(Some(MediaType("text", "plain", Map("charset" -> "ISO-8859-1"))))
    }*/
    "generated correctly" in {
      MediaType("application", "xml", Map("charset" -> "ISO-8859-1")).toString must be equalTo("application/xml; charset=ISO-8859-1")
    }
    "multiple arguments must be generated correctly" in {
      MediaType("application", "atom+xml", Map("charset" -> "ISO-8859-1", "type" -> "entry")).toString must be equalTo("application/atom+xml; charset=ISO-8859-1; type=entry")
    }

    "Incorrectly formatted string must fail" in {
      MediaType("application") must be equalTo(None)
    }
  }
}

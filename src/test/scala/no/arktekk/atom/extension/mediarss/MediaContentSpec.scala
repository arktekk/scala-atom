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

package no.arktekk.atom.extension.mediarss

import org.specs2.mutable.Specification
import java.net.URI
import no.arktekk.atom.MediaType

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
class MediaContentSpec extends Specification {
  "media content" should {
    "be created correct using 'image' factory" in {
      val uri = URI.create("http://example.com/image/123.jpeg")
      val mediaType = MediaType("image/jpeg")
      val mc = MediaContent.image(uri, mediaType)
      mc.url must beEqualTo(uri)
      mc.mediaType must beEqualTo(mediaType)
    }

    "be created correct using 'image' factory with width and height" in {
      val uri = URI.create("http://example.com/image/123.jpeg")
      val mediaType = MediaType("image/jpeg")
      val mc = MediaContent.image(uri, mediaType, 800, 600)
      mc.url must beEqualTo(uri)
      mc.mediaType must beEqualTo(mediaType)
      mc.width must beEqualTo(Some(800))
      mc.height must beEqualTo(Some(600))
    }
  }

}

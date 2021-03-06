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

import org.specs2.mutable.Specification

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
class PolygonSpec extends Specification {
  "A polygon" should {
    "be generated correctly" in {
      val expected = "45.256 -110.45 46.46 -109.48 43.84 -109.86 45.256 -110.45"
      val polygon = Polygon(List(Point(45.256, -110.45), Point(46.46, -109.48), Point(43.84, -109.86), Point(45.256, -110.45)))
      polygon.toValue("###.#####") must beEqualTo(expected)
    }
    "be parsed correctly" in {
      val input = "45.256 -110.45 46.46 -109.48 43.84 -109.86 45.256 -110.45"
      val expected = Polygon(Point(45.256, -110.45), Point(46.46, -109.48), Point(43.84, -109.86))
      Polygon(input) should beEqualTo(Some(expected))
    }
  }
}

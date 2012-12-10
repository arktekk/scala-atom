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

import com.codecommit.antixml._
import java.net.URI

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
trait PersonLike extends ElementWrapper {

  def name = (wrapped \ atomSelector("name") \ text).head

  def email = (wrapped \ atomSelector("email") \ text).headOption

  def url = (wrapped \ atomSelector("url") \ text).headOption

  def withName(name: String) = replace("name", name)

  def withEmail(email: String) = replace("email", email)

  def withUrl(url: URI) = replace("url", url.toString)

  private def replace(name: String, value: String) = {
    replaceChildren(atomSelector(name), Group(atomTextElem(name, value)))
  }
}

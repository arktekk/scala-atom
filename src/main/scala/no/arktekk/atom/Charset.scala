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

import java.nio.charset.{Charset => JCharset}
import io.Codec

/**
 * @author Erlend Hamnaberg<erlend.hamnaberg@arktekk.no>
 */

case class Charset(wrapped: JCharset) {
  def name = wrapped.name()
}

object Charset {
  implicit val defaultCharset = Charset(Codec.UTF8)

  def forName(name: String) = Charset(JCharset.forName(name))

  implicit def toCharset(name: String): Charset = forName(name)
  implicit def toCharset(charset: JCharset): Charset = Charset(charset)
}
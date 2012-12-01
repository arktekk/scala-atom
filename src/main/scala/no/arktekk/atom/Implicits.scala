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

import org.joda.time.format.DateTimeFormat
import org.joda.time.DateTime
import java.net.URI
import scala.util.control.Exception._
import com.codecommit.antixml._
import Atom._

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
trait Implicits {

  private val dateTimeFormat = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'").withZoneUTC()
  private val dateTimeFormatNoMillis = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss'Z'").withZoneUTC()
  private val dateTimeFormatWithZone = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss.SSSZ")
  private val dateTimeFormatWithZoneNoMillis = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ssZ")

  def parseDateTime(input: String): DateTime = {
    val dt = allCatch.opt(dateTimeFormat.parseDateTime(input)) orElse
      allCatch.opt(dateTimeFormatNoMillis.parseDateTime(input)) orElse
      allCatch.opt(dateTimeFormatWithZone.parseDateTime(input)) orElse
      allCatch.opt(dateTimeFormatWithZoneNoMillis.parseDateTime(input))
    dt.getOrElse(throw new IllegalArgumentException(input + " is not a valid Atom date"))
  }

  def dateTimeToString(dateTime: DateTime) = dateTimeFormat.print(dateTime)

  implicit def string2URI(iri: String): URI = URI.create(iri)

  implicit def string2TextConstruct(text: String): TextConstruct = TextConstruct.Textual(text)

}

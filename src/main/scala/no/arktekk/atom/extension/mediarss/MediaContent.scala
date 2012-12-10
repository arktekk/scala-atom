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

import java.net.URI
import java.util.Locale
import org.joda.time.Seconds
import no.arktekk.atom._
import extension.Extensible

/**
 * http://www.rssboard.org/media-rss#media-content
 *
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
//support expression attribute
trait MediaContent extends Extensible[MediaContent] {

  //TODO: support type attribute
  def description: Option[String]

  def url: URI

  def fileSize: Long

  def mediaType: Option[MediaType]

  def medium: Option[Medium]

  def isDefault: Boolean

  def width: Option[Int]

  def height: Option[Int]

  def bitrate: Option[Int]

  def framerate: Option[Int]

  def samplingrate: Option[Double]

  def channels: Option[Int]

  def duration: Option[Seconds]

  def lang: Option[Locale]

  def withUrl(uri: URI): MediaContent

  def withFileSize(input: Long): MediaContent

  def withMediaType(input: MediaType): MediaContent

  def withMedium(input: Medium): MediaContent

  def withDefault(b: Boolean): MediaContent

  def withWidth(input: Int): MediaContent

  def withHeight(input: Int): MediaContent

  def withBitrate(input: Int): MediaContent

  def withFramerate(input: Int): MediaContent

  def withSamplingRate(input: Int): MediaContent

  def withChannels(input: Int): MediaContent

  def withDuration(input: Seconds): MediaContent

  def withLang(input: Locale): MediaContent

  def withDescription(description: String): MediaContent
}

sealed abstract class Medium(val value: String) {
  override def toString = value
}


object Medium {
  def apply(input: String): Medium = input.toLowerCase(Locale.ENGLISH) match {
    case IMAGE.value => IMAGE
    case VIDEO.value => VIDEO
    case AUDIO.value => AUDIO
    case DOCUMENT.value => DOCUMENT
    case EXECUTABLE.value => EXECUTABLE
    case _ => sys.error("Unexpected value for medium attribute")
  }

  case object IMAGE extends Medium("image")

  case object AUDIO extends Medium("audio")

  case object VIDEO extends Medium("video")

  case object DOCUMENT extends Medium("document")

  case object EXECUTABLE extends Medium("executable")

}

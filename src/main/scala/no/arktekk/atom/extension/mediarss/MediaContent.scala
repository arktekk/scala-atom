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

import com.codecommit.antixml._
import no.arktekk.atom.Atom._
import java.net.URI
import java.util.Locale
import org.joda.time.Seconds
import no.arktekk.atom.{NamespacedName, BasicElementWrapper, MediaType, ElementWrapper}
import no.arktekk.atom.extension.SimpleTextElementWrapper

/**
 * http://www.rssboard.org/media-rss#media-content
 *
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
//support expression attribute
case class MediaContent(wrapped: Elem) extends ElementWrapper {
  type T = MediaContent

  protected def self = this

  def copy(elem: Elem) = new MediaContent(elem)

  //TODO: support type attribute
  def description = (wrapped \ namespaceSelector(MediaRSSConstants.ns, "description") \ text)

  def url = wrapped.attrs.get("url").map(URI.create(_)).get

  def fileSize = wrapped.attrs.get("fileSize").map(_.toLong)

  def mediaType = wrapped.attrs.get("type").flatMap(MediaType(_))

  def medium = wrapped.attrs.get("medium").map(Medium(_))

  def isDefault = wrapped.attrs.get("isDefault").map(_.toBoolean)

  def width = wrapped.attrs.get("width").map(_.toInt)

  def height = wrapped.attrs.get("height").map(_.toInt)

  def bitrate = wrapped.attrs.get("bitrate").map(_.toInt)

  def framerate = wrapped.attrs.get("framerate").map(_.toInt)

  def samplingrate = wrapped.attrs.get("samplingrate").map(_.toDouble)

  def channels = wrapped.attrs.get("channels").map(_.toInt)

  def duration = wrapped.attrs.get("duration").map(_.toInt).map(Seconds.seconds(_))

  def lang = wrapped.attrs.get("duration").map(new Locale(_))

  def withUrl(uri: URI) = withAttribute("url", uri)

  def withFileSize(input: Long) = withAttribute("fileSize", input)

  def withMediaType(input: MediaType) = withAttribute("type", input)

  def withMedium(input: Medium) = withAttribute("medium", input)

  def withDefault(b: Boolean) = withAttribute("isDefault", b)

  def withWidth(input: Int) = withAttribute("width", input)

  def withHeight(input: Int) = withAttribute("height", input)

  def withBitrate(input: Int) = withAttribute("bitrate", input)

  def withFramerate(input: Int) = withAttribute("framerate", input)

  def withSamplingRate(input: Int) = withAttribute("samplingrate", input)

  def withChannels(input: Int) = withAttribute("channels", input)

  def withDuration(input: Seconds) = withAttribute("seconds", input.getSeconds)

  def withLang(input: Locale) = withAttribute("lang", input.getLanguage)

  def withDescription(description: String) =
    addChild(SimpleTextElementWrapper(NamespacedName(MediaRSSConstants.ns, MediaRSSConstants.prefix, "description"), description))
}

object MediaContent {
  def apply(): MediaContent = {
    val wrapper = BasicElementWrapper.withName(NamespacedName(MediaRSSConstants.ns, MediaRSSConstants.prefix, "content"))
    MediaContent(wrapper.wrapped)
  }

  def image(href: URI, mediaType: Option[MediaType]): MediaContent = {
    val attrs = Map((QName(None, "url") -> href.toString), (QName(None, "medium") -> Medium.IMAGE.toString)) ++
      mediaType.map((QName(None, "type") -> _.toString)).toSeq
    val wrapper = BasicElementWrapper.withNameAndAttributes(
      NamespacedName(MediaRSSConstants.ns, MediaRSSConstants.prefix, "content"),
      new Attributes(attrs))
    MediaContent(wrapper.wrapped)
  }

  def image(href: URI, mediaType: Option[MediaType], width: Int, height: Int): MediaContent = {
    image(href, mediaType).withWidth(width).withHeight(height)
  }
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

/*
 * Copyright 2011 Arktekk AS
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

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
class MediaType private(val major: String, val minor:String, val params: Map[String, String]) {
  override def toString = "%s/%s".format(major, minor) + params.mkString(";")
}

object MediaType {
  val ALL = MediaType("*/*")
  val IMAGE_JPEG = MediaType("image/jpeg")
  val IMAGE_PNG = MediaType("image/png")

  def apply(mt: String): MediaType = {
    import javax.activation.MimeType
    import scala.collection.JavaConversions._
    val mimeType = new MimeType(mt)
    val names = mimeType.getParameters.getNames.asInstanceOf[java.util.Enumeration[String]]
    val params = names.foldLeft(Map[String, String]())((acc, p) => acc + (p -> mimeType.getParameter(p)))
    new MediaType(mimeType.getPrimaryType, mimeType.getSubType, params)
  }

  def unapply(mediaType: MediaType) = Some((mediaType.major, mediaType.minor, mediaType.params))
}

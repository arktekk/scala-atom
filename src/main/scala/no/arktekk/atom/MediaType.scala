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
case class MediaType(major: String, minor: String, params: Map[String, String] = Map.empty) {
  override def toString = "%s/%s".format(major, minor) + params.map{case (a, b) => "; %s=%s".format(a, b)}.mkString("")
}

object MediaType {
  val ALL = MediaType("*", "*")
  val IMAGE_JPEG = MediaType("image", "jpeg")
  val IMAGE_PNG = MediaType("image", "png")
  val ATOM = MediaType("application", "atom+xml")
  val ATOM_ENTRY = MediaType("application", "atom+xml", Map("type" -> "entry"))
  val CATEGORY = MediaType("application", "atomcat+xml")
  val SERVICE = MediaType("application", "atomsvc+xml")

  def apply(mt: String): Option[MediaType] = {
    import javax.activation.MimeType
    import scala.collection.JavaConverters._
    try {
      val mimeType = new MimeType(mt)
      val names = mimeType.getParameters.getNames.asInstanceOf[java.util.Enumeration[String]].asScala
      val params = names.foldLeft(Map[String, String]())((acc, p) => acc + (p -> mimeType.getParameter(p)))
      Some(new MediaType(mimeType.getPrimaryType, mimeType.getSubType, params))
    }
    catch {
      case _: Exception => None
    }
  }
}

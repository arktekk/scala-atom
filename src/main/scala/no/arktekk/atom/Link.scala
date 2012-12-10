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

import java.net.URI
import com.codecommit.antixml._

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
trait Link {

  def href: URI

  def rel: Option[String]

  def mediaType: Option[MediaType]

  def title: Option[String]

  def withTitle(title: String): Link

  def withHref(href: URI):Link

  def withRel(rel: String):Link

  def withMediaType(mt: MediaType): Link
}

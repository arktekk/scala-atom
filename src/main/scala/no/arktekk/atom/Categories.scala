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

import extension.Extensible
import java.net.URI

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
trait Categories extends Extensible[Categories] with DocumentLike {

  def href: URI

  def withHref(uri: URI)

  def fixed: Boolean

  def scheme: Option[String]

  def categories: Seq[Category]

  def addCategory(cat: Category): Categories

  def withCategories(cats: Seq[Category]): Categories

  def withFixed(fixed: Boolean): Categories

  def withScheme(scheme: String): Categories
}

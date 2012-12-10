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

import extension.Extensible

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
trait Category extends Extensible[Category] {

  def scheme: String

  def term: String

  def label: Option[String]

  def withScheme(sch: String): Category

  def withTerm(term: String): Category

  def withLabel(label: String): Category

}


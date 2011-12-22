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

package no.arktekk

import java.net.URI

package object atom {
  implicit def string2IRI(iri: String) : IRI = IRI(URI.create(iri))
  implicit def uri2IRI(uri: URI) : IRI = IRI(uri)
  implicit def string2TextConstruct(text: String) : TextConstruct = TextConstruct.Textual(text)
}

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

package no.arktekk.atom.extension.georss

import no.arktekk.atom.ElementWrapper
import com.codecommit.antixml.{NamespaceBinding, QName}


/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
case class FeatureType(name: String) {
  def toXML = ElementWrapper.withNameAndText(NamespaceBinding(GeorssConstants.prefix, GeorssConstants.ns), "featureTypeTag", name)
}

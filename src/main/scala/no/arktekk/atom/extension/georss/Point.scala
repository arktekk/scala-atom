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

import java.text.DecimalFormat
import com.codecommit.antixml.{NamespaceBinding, QName}
import no.arktekk.atom.ElementWrapper

/**
 * http://georss.org/simple
 *
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
case class Point(lat: Double, lon: Double) {

  def toValue(format: String) = {
    val formatter = new DecimalFormat(format)
    "%s %s".format(formatter.format(lat), formatter.format(lon))
  }

  def toXML(format: String) = {
    ElementWrapper.withNameAndText(NamespaceBinding(GeorssConstants.prefix, GeorssConstants.ns), "point", toValue(format))
  }
}

object Point {
  def apply(input: String): Option[Point] = {
    Some(input).map(_.split(" ", 2)).filter(_.length == 2).map{case Array(x, y) => Point(x.toDouble, y.toDouble)}
  }
}

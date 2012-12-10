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


/**
 * http://georss.org/simple
 *
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
case class Circle(point: Point, radius: Int) {
  def toValue(format: String) = point.toValue(format) + " " + radius
}

object Circle {

  def apply(value: String): Option[Circle] = {
    val grouped = value.trim.split(" ").grouped(2)
    grouped.toList match {
      case List(Array(x,y), Array(z)) => Some(Circle(Point(x.toDouble, y.toDouble), z.toInt))
      case _ => None
    }
  }
}

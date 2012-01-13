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
case class Line private[georss](points: Seq[Point]) {
  def toValue(format: String) = points.map(_.toValue(format)).mkString(" ")
}

object Line {
  def apply(first: Point, second: Point, rest: Point*): Line = {
    apply(List(first, second) ++ rest)
  }

  def apply(value: String): Option[Line] = {
    val seq = value.trim.split(" ").grouped(2).map{
      case Array(x, y) => Point(x.toDouble, y.toDouble)
      case x => sys.error("Unexpected length of array, expected 2 but was: " + x.length)
    }.toList
    if (seq.length >= 2) Some(Line(seq)) else None
  }
}

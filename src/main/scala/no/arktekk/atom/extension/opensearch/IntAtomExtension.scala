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
package no.arktekk.atom.extension.opensearch

import no.arktekk.atom.extension.opensearch.OpensearchConstants._
import no.arktekk.atom.extension.OptionSelectableElementWrapperAtomExtension
import com.codecommit.antixml._
import no.arktekk.atom._

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
private[opensearch] class IntAtomExtension(name:String) extends OptionSelectableElementWrapperAtomExtension[FeedLike, Int] {
  protected val selector: Selector[Elem] = NSRepr(ns) -> name

  protected def function = (e) => (e \ text).headOption.map(_.toInt).get

  def toChildren(a: Option[Int]) = {
    a.map(x => ElementWrapper.withNameAndText(NamespaceBinding(prefix, ns), name, x.toString)).toIndexedSeq
  }
}

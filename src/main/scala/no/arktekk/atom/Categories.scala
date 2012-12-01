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

import com.codecommit.antixml._
import java.net.URI

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
case class Categories private[atom] (wrapped: Elem) extends ElementWrapper {
  require(Elem.validateNamespace(wrapped, Atom.atompubNamespace), "Wrong namespace defined")
  require(wrapped.name == "categories", "Wrong name of element")

  def href: URI = wrapped.attrs.get("href").map(URI.create(_)).get

  def withHref(uri: URI) = withAttribute("href", uri.toString)

  def fixed: Boolean = wrapped.attrs.get("fixed").map(f => f == "yes").getOrElse(false)

  def scheme: Option[String] = wrapped.attrs.get("scheme")

  def categories: Seq[Category] = (wrapped \ atomSelector("category")).map(Category(_))

  def addCategory(cat: Category) = addChild(cat)

  def withCategories(cats: Seq[Category]) = replaceChildren(atomSelector("category"), cats)

  def withFixed(fixed: Boolean) = withAttribute("fixed", if (fixed) "yes" else "no")

  def withScheme(scheme: String) = withAttribute("scheme", scheme)

  type T = Categories

  protected def self = this

  def copy(elem: Elem) = new Categories(elem)
}

object Categories {
  def apply(): Categories = apply(Elem(NamespaceBinding("app", Atom.atompubNamespace), "categories")).
    addNamespace(Some(""), Atom.namespace)

  def apply(href: URI): Categories = apply().withHref(href)
}

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

import io.{Source => IOSource}
import com.codecommit.antixml._

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
object Atom {
  val namespace = "http://www.w3.org/2005/Atom"
  val atompubNamespace = "http://www.w3.org/2007/app"
  val atom = NamespaceBinding(namespace)
  val atompub = NamespaceBinding(atompubNamespace)

  def parseFeed(src: IOSource): Either[Exception, Feed] = parseDocument(src, "feed", namespace, Feed(_))

  def parseEntry(src: IOSource): Either[Exception, Entry] = parseDocument(src, "entry", namespace, Entry(_))

  def parseService(src: IOSource): Either[Exception, Service] = parseDocument(src, "service", atompubNamespace, Service(_))

  def parseCategories(src: IOSource): Either[Exception, Categories] = parseDocument(src, "categories", atompubNamespace, Categories(_))

  def parseDocument[A](src: IOSource, root: String, namespace: String, f: (Elem) => A) = {
    try {
      val elem = XML.fromSource(src)
      elem match {
        case e@Elem(p, `root`, _, nb, _) if (Elem.validateNamespace(e, namespace)) => Right(f(e))
        case e => Left(new IllegalArgumentException("unexpected XML here:\n%s".format(e)))
      }
    }
    catch {
      case e: Exception => Left(e)
    }
  }
}

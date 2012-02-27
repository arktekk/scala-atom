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
  val namespaces: Map[String, String] = Map(("", Atom.namespace))


  def parse[A <: Base](src: IOSource): Either[Exception, A] = {
    try {
      val elem = XML.fromSource(src)
      elem match {
        case e@Elem(_, "feed", _, _, _) if (e.scope.find {
          case (_, ns) => ns == namespace
        }.isDefined) => Right(Feed(e).asInstanceOf[A])
        case e@Elem(_, "entry", _, _, _) if (e.scope.find {
          case (_, ns) => ns == namespace
        }.isDefined) => Right(Entry(e).asInstanceOf[A])
        case e => Left(new IllegalArgumentException("unexpected XML here:\n%s".format(e)))
      }
    }
    catch {
      case e: Exception => Left(e)
    }
  }

  def parseService(src: IOSource): Either[Exception, Service] = {
    try {
      val elem = XML.fromSource(src)
      elem match {
        case e@Elem(_, "service", _, _, _) if (e.scope.find {
          case (_, ns) => ns == atompubNamespace
        }.isDefined) => Right(Service(e))
        case e => Left(new IllegalArgumentException("unexpected XML here:\n%s".format(e)))
      }
    }
    catch {
      case e: Exception => Left(e)
    }
  }
}

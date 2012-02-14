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

import java.net.URI
import collection.immutable.Map
import io.{Source => IOSource}
import com.codecommit.antixml._
import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormatter, DateTimeFormat}

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
object Atom {
  val namespace = "http://www.w3.org/2005/Atom"
  val atompubNamespace = "http://www.w3.org/2007/app"
  val namespaces: Map[String, String] = Map(("", namespace))
  private val dateTimeFormat = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'").withZoneUTC()
  private val dateTimeFormatNoMillis = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss'Z'").withZoneUTC()

  def parseDateTime(input: String) = {
    val formatters = List(dateTimeFormat, dateTimeFormatNoMillis)

    def format(formatter: DateTimeFormatter): Either[Exception, DateTime] = {
      try {
        Right(formatter.parseDateTime(input))
      }
      catch {
        case e: Exception => Left(e)
      }
    }

    val parser = formatters.map(format(_))
    val right = parser.find(_.isRight)
    right.getOrElse(Left(throw new IllegalArgumentException(input + "Not a valid Atom date"))) match {
      case Right(e) => e
      case Left(e) => throw e
    }
  }
  
  def dateTimeToString(dateTime: DateTime) = dateTimeFormat.print(dateTime)

  private[atom] def onlyElementName(name: String) = {
    Elem(None, name, Attributes(), namespaces, Group.empty)
  }

  private[atom] def simple(name: String, value: String, attr: Attributes = Attributes()) = {
    withChildren(name, attr, Seq(Text(value)))
  }

  private[atom] def withChildren(name: String, attr: Attributes = Attributes(), children: Seq[Node], prefix: Option[String] = None) = {
    Elem(None, name, attr, namespaces, Group.empty ++ children)
  }

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
        case e => Left(new IllegalArgumentException("unknown XML here: %s".format(e)))
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
        case e => Left(new IllegalArgumentException("unknown XML here: %s".format(e)))
      }
    }
    catch {
      case e: Exception => Left(e)
    }
  }

  implicit def string2URI(iri: String): URI = URI.create(iri)

  implicit def string2TextConstruct(text: String): TextConstruct = TextConstruct.Textual(text)

  def namespaceSelector(namespace: String, element: String): Selector[Elem] = new Selector[Elem] {
    def apply(node: Node) = node match {
      case e: Elem => e
      case _ => sys.error("woot?!")
    }

    def isDefinedAt(node: Node) = node match {
      case e@Elem(prefix, `element`, _, scopes, _) =>
        val x = scopes.find({
          case (_, `namespace`) => true;
          case _ => false
        }).map(_._1)
        prefix.orElse(Some("")).equals(x)
      case _ =>
        false
    }
  }

  //TODO: Remove this when AntiXML Fixes mapping based on namespaces.
  def prefixAndElementSelector(prefix: String, element: String): Selector[Elem] = {
    Selector({case e@Elem(Some(`prefix`), `element`, _, _, _) => e})
  }
}

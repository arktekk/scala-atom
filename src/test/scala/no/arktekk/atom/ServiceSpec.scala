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

import org.specs2.mutable.Specification

/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 */
class ServiceSpec extends Specification {
  "A Service document" should {
    "parse correctly" in {
      val service: Either[Exception, Service] = Atom.parseService(io.Source.fromInputStream(getClass.getResourceAsStream("/service.xml")))
      service must beRight[Service]
    }
    "find correct values in service" in {
      val service: Service = Atom.parseService(io.Source.fromInputStream(getClass.getResourceAsStream("/service.xml"))).right.get
      service.workspaces.length must beEqualTo(2)
      service.workspaces.head.title.get.toString must beEqualTo("Main Site")
    }
    "fail parsing when namespace is fux0red" in {
      val service: Either[Exception, Service] = Atom.parseService(io.Source.fromInputStream(getClass.getResourceAsStream("/service-with-incorrect-ns.xml")))
      service should beLeft[Exception]
    }
  }
}

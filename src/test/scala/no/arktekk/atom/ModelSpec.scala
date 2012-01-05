package no.arktekk.atom

import org.specs2.mutable.Specification
import java.net.URI
import org.joda.time.DateTime
import com.codecommit.antixml.{Group, Attributes, Elem}

/**
 * Created by IntelliJ IDEA.
 * User: maedhros
 * Date: 1/4/12
 * Time: 8:56 AM
 * To change this template use File | Settings | File Templates.
 */

class ModelSpec extends Specification {
  "A model" should {
    "autogenerate prefix for default prefix" in {
      val entry = Entry(URI.create("hei"), "Title", new DateTime())
      val entryWithNamespace = entry.addNamespace("", "urn:foo:bar")
      val scope = entryWithNamespace.wrapped.scope
      scope.get("ns1") mustEqual Some("urn:foo:bar")
    }
    "return self when we already have the namespace registered" in {
      val entry = Entry(URI.create("hei"), "Title", new DateTime())
      val entryWithNamespace = entry.addNamespace("", "urn:foo:bar")
      val entryWitchShouldBeTheSame = entryWithNamespace.addNamespace("ns2", "urn:foo:bar")
      val scope1 = entryWithNamespace.wrapped.scope
      val scope2 = entryWitchShouldBeTheSame.wrapped.scope

      scope1 should beTheSameAs(scope2)
    }
    "fail create when someone has created something weird" in {
      Entry(Elem(None, "entry", Attributes(), Map(), Group.empty)) must throwA[IllegalArgumentException]
    }
  }
}

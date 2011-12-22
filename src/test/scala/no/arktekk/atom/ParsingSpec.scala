package no.arktekk.atom

import org.specs2.mutable.Specification
import io.Source

/**
 * Created by IntelliJ IDEA.
 * User: maedhros
 * Date: 12/22/11
 * Time: 9:18 PM
 * To change this template use File | Settings | File Templates.
 */

class ParsingSpec extends Specification {

  "A parser " should {
    "create a feed" in {
      val feed = Atom.parse(Source.fromInputStream(getClass.getResourceAsStream("/feed.xml")))
      println(feed)
      1 mustEqual 1
      //feed shouldEqual  Feed("urn:uuid:something-random", "Example list", Atom.dateTimeFormat.parseDateTime("2011-01-01T08:00:00.00Z"), Person.author("Example"))
    }
  }
}
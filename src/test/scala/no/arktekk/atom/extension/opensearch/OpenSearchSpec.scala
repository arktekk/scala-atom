package no.arktekk.atom.extension.opensearch

import org.specs2.mutable.Specification
import io.{Source => IOSource}
import no.arktekk.atom.{Person, Feed, Atom}
import org.joda.time.DateTime

/**
 * Created by IntelliJ IDEA.
 * User: maedhros
 * Date: 1/5/12
 * Time: 12:45 PM
 * To change this template use File | Settings | File Templates.
 */

class OpenSearchSpec extends Specification {
  "A parser that supports opensearch" should {
    "parse correctly" in {
      val feed : Feed = Atom.parse(IOSource.fromInputStream(getClass.getResourceAsStream("/extensions/opensearch.xml"))).right.get
      feed must not be null
    }

    "Find correct extensions" in {
      val feed : Feed = Atom.parse(IOSource.fromInputStream(getClass.getResourceAsStream("/extensions/opensearch.xml"))).right.get
      OpenSearchResponseAtomExtension.fromLike(feed).itemsPerPage must be equalTo(Some(10))
    }
    "Add extensions" in {
      val feed : Feed = Feed("Hei og haa", new DateTime(), Person.author("me"))

      var other = feed.apply(OpenSearchResponseAtomExtension, OpenSearchResponse(totalResults = Some(1)))

      feed must not beTheSameAs(other)

      other = feed.apply(OpenSearchResponseAtomExtension, OpenSearchResponse(totalResults = Some(1)))

      feed must not beTheSameAs(other)

      other = feed.apply(OpenSearchResponseAtomExtension, OpenSearchResponse(totalResults = Some(1)))

      feed must not beTheSameAs(other)

      other = feed.apply(OpenSearchResponseAtomExtension, OpenSearchResponse(Seq(Query(Role.REQUEST))))

      feed must not beTheSameAs(other)
    }
  }
}

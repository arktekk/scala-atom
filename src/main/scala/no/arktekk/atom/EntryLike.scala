package no.arktekk.atom

import org.joda.time.DateTime
import no.arktekk.atom.Atom._
import com.codecommit.antixml._
import com.codecommit.antixml.Selector._

/**
 * Created by IntelliJ IDEA.
 * User: maedhros
 * Date: 1/4/12
 * Time: 9:52 AM
 * To change this template use File | Settings | File Templates.
 */

private[atom] trait EntryLike extends AtomLike {
  def published: Option[DateTime] = (wrapped \ "published" \ text).headOption.map(dateTimeFormat.parseDateTime(_))

  def content: Option[Content] = (wrapped \ "content").headOption.flatMap(Content(_))

  def summary: Option[Content] = (wrapped \ "summary").headOption.flatMap(Content(_))

  def withPublished(published: DateTime) = copy(removeChild("published").copy(children = wrapped.children ++ List(simple("published", dateTimeFormat.print(published)))))

  def withSummary(summary: Content) = copy(removeChild("summary").copy(children = wrapped.children ++ List(summary.toXML("summary"))))

  def withContent(content: Content) = copy(removeChild("content").copy(children = wrapped.children ++ List(content.toXML("content"))))

  def removeContent() = copy(removeChild("content"))

  def removeSummary() = copy(removeChild("summary"))
}

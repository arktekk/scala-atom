package no.arktekk.atom

import java.net.URI
import no.arktekk.atom.Atom._
import com.codecommit.antixml._
import com.codecommit.antixml.Selector._

/**
 * Created by IntelliJ IDEA.
 * User: maedhros
 * Date: 1/4/12
 * Time: 9:50 AM
 * To change this template use File | Settings | File Templates.
 */

private[atom] trait FeedLike extends AtomLike {
  def subtitle: Option[TextConstruct] = (wrapped \ "subtitle").headOption.flatMap(TextConstruct(_))

  def entries: List[Entry] = (wrapped \ "entry").map(Entry(_)).toList

  def logo = (wrapped \ "logo" \ text).headOption.map(URI.create(_))

  def icon = (wrapped \ "icon" \ text).headOption.map(URI.create(_))

  def withSubtitle(title: TextConstruct) = copy(removeChild("subtitle").copy(children = wrapped.children ++ List(title.toXML("subtitle"))))

  def withLogo(logo: URI) = copy(removeChild("logo").copy(children = wrapped.children ++ List(simple("logo", logo.toString))))

  def withIcon(icon: URI) = copy(removeChild("icon").copy(children = wrapped.children ++ List(simple("icon", icon.toString))))
}

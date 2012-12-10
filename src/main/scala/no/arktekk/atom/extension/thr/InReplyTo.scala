package no.arktekk.atom.extension.thr

import java.net.URI
import no.arktekk.atom.MediaType

trait InReplyTo {

  def ref: URI

  def href: Option[URI]

  def source: Option[URI]

  def mediaType: Option[MediaType]

  def withRef(uri: URI): InReplyTo

  def withHref(uri: URI): InReplyTo

  def withSource(uri: URI): InReplyTo

  def withMediaType(mt: MediaType): InReplyTo

}

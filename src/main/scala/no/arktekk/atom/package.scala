package no.arktekk

import java.net.URI

package object atom {
  implicit def string2IRI(iri: String) : IRI = IRI(URI.create(iri))
  implicit def uri2IRI(uri: URI) : IRI = IRI(uri)
  implicit def string2TextConstruct(text: String) : TextConstruct = TextConstruct.Textual(text)
}

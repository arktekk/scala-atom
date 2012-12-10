package no.arktekk

import com.codecommit.antixml._

package object atom extends Implicits with AtomSelectors {

  def atomTextElem(name: String, text: String) =
    Elem(Atom.atom, name, Attributes(), Group[Node](Text(text)))

  def appTextElem(name: String, text: String) =
    Elem(NamespaceBinding("app", Atom.atompubNamespace), name, Attributes(), Group[Node](Text(text)))

}

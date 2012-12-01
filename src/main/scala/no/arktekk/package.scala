package no.arktekk

import com.codecommit.antixml.QName

package object atom extends Implicits with AtomSelectors {
  type Attribute = (QName, String)
}

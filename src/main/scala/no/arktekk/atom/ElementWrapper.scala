package no.arktekk.atom

import com.codecommit.antixml._

/**
 * Created by IntelliJ IDEA.
 * User: maedhros
 * Date: 1/2/12
 * Time: 11:46 AM
 * To change this template use File | Settings | File Templates.
 */

abstract class ElementWrapper(elem: Elem) {
  final def getAttribute(name: String) = elem.attrs.get(name)

  final def query(name: String): Group[Node] = elem \ name
}

package no.arktekk.atom

import org.specs2.mutable.Specification
import io.Source
import com.codecommit.antixml.Elem

/**
 * Created by IntelliJ IDEA.
 * User: maedhros
 * Date: 1/2/12
 * Time: 12:53 PM
 * To change this template use File | Settings | File Templates.
 */

class ExtensionSpec extends Specification {
  "A parser that handles extensions" should {
    "parse correctly" in {
      val entry : Entry = Atom.parse(Source.fromInputStream(getClass.getResourceAsStream("/entry-with-extension.xml")))
      entry must not be null
    }
    "find extension in parsed" in {
      val entry : Entry = Atom.parse(Source.fromInputStream(getClass.getResourceAsStream("/entry-with-extension.xml")))
      val simple = entry.getExtension[SimpleExtension](Namespaced("urn:ext:ext", "hello"))
      simple mustEqual Some(SimpleExtension((entry.wrapped \ "hello").head.asInstanceOf[Elem]))
    }
  }
}

case class SimpleExtension(wrapped: Elem) extends ElementWrapper(wrapped)

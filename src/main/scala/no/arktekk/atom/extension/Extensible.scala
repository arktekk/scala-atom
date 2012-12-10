package no.arktekk.atom.extension

trait Extensible[T <: Extensible[_]] { self : T =>
  def extensions: IndexedSeq[Extension]

  def collect[B](selector: PartialFunction[Extension, B]) = extensions.collect(selector)

  def attributeExtensions: IndexedSeq[AttributeExtension]

  def apply[B](ext: AtomExtension[T, B], value: B): T = {
    val attrs = ext.toAttributes(value)
    val copy = addAttributeExtensions(attrs).asInstanceOf[T]
    copy.addExtensions(ext.toChildren(value)).asInstanceOf[T]
  }

  def unapply[B](ext: AtomExtension[T, B]): Option[B] = Some(ext.fromLike(this))

  def addExtensions(seq: IndexedSeq[Extension]): T

  def addAttributeExtensions(attrs: IndexedSeq[AttributeExtension]): T
}
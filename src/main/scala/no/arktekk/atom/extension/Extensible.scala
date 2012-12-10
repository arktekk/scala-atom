package no.arktekk.atom.extension

trait Extensible[T <: Extensible[_]] { self : T =>
  def extensions: IndexedSeq[Extension]

  def attributeExtensions: IndexedSeq[AttributeExtension]

  def apply[B](ext: AtomExtension[T, B], value: B): T = {
    val attrs = ext.toAttributes(value)
    val copy: T = addAttributeExtensions(attrs).asInstanceOf[T]
    copy.addExtensions(ext.toChildren(value)).asInstanceOf[T]
  }

  def extract[B](ext: AtomExtension[T, B]): B = ext.fromLike(self)

  def addExtensions(seq: IndexedSeq[Extension]): T

  def addAttributeExtensions(attrs: IndexedSeq[AttributeExtension]): T
}

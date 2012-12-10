package no.arktekk.atom.extension

sealed trait Extension {
  def namespace: String
  def name: String
}

trait ElemExtension extends Extension {
  def children: Seq[Extension]
}

trait StringExtension extends Extension {
  def value: String
}

trait AttributeExtension {
  def prefix: Option[String]
  def name: String
  def value: String
}

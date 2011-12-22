package no.arktekk.atom

/**
 * Created by IntelliJ IDEA.
 * User: maedhros
 * Date: 12/20/11
 * Time: 11:43 AM
 * To change this template use File | Settings | File Templates.
 */

class MediaType private(val major: String, val minor:String, val params: Map[String, String]) {
  override def toString = "%s/%s".format(major, minor) + params.mkString(";")
}

object MediaType {
  val ALL = MediaType("*/*")
  val IMAGE_JPEG = MediaType("image/jpeg")
  val IMAGE_PNG = MediaType("image/png")

  def apply(mt: String): MediaType = {
    import javax.activation.MimeType
    import scala.collection.JavaConversions._
    val mimeType = new MimeType(mt)
    val names = mimeType.getParameters.getNames.asInstanceOf[java.util.Enumeration[String]]
    val params = names.foldLeft(Map[String, String]())((acc, p) => acc + (p -> mimeType.getParameter(p)))
    new MediaType(mimeType.getPrimaryType, mimeType.getSubType, params)
  }

  def unapply(mediaType: MediaType) = Some((mediaType.major, mediaType.minor, mediaType.params))
}

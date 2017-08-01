import scala.io.Source

object Utils {
  def readTextFromResource(source: String): Option[String] =
    try { Some(Source.fromURL(getClass.getResource(source)).mkString) } catch { case _ => None }
}

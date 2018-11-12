package nl.elmar.xml.reader

import cats.{ Show, Traverse}
import cats.instances.list._

case class XmlPath(path: List[String]) {
  def  \ (child: String): XmlPath = copy(path :+ child)

  def read[A](implicit reader: Reader[A]) = Reader( nodeSeq =>
    reader.run(path.foldLeft(nodeSeq)(_ \ _))
      .leftMap(
        _.map( readerError =>
          readerError.copy(
            path = XmlPath(path ++ readerError.path.path))))
  )

  def list[A](implicit reader: Reader[A]): Reader[List[A]] = read(
    Reader( nodeSeq =>
      Traverse[List].sequence(nodeSeq.toList map reader.run)
    )
  )

  def optional[A](implicit reader: Reader[A]): Reader[Option[A]] =
    list[A].andThen(r => valid(r.headOption))

  def withDefault[A](default: A)(implicit reader: Reader[A]): Reader[A] =
    optional[A].map(_.getOrElse(default))

  def first[A](implicit reader: Reader[A]): Reader[A] =
    optional[A].andThen(_ map valid getOrElse invalid(s"node not found", this))

  @inline def apply[A](implicit reader: Reader[A]): Reader[A] = read[A]
}

object XmlPath {
  val __ = XmlPath(Nil)

  implicit val xmlPathShow: Show[XmlPath] =
    Show.show( path =>
      path.path.mkString("__\\", "\\", "")
    )
}


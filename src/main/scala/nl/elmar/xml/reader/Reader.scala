package nl.elmar.xml.reader

import cats.data.Kleisli

import scala.xml.NodeSeq

object Reader {
  def apply[A](f: NodeSeq => Result[A]): Reader[A] = Kleisli(f)

  def pure[A](a: A): Reader[A] = Reader(_ => valid(a))

  def reader[A: Reader]: Reader[A] = implicitly[Reader[A]]

}

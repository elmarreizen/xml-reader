package com.github.lavrov.xml.reader

import cats.data.Kleisli

import scala.xml.NodeSeq

object Reader {
  def apply[A](f: NodeSeq => Result[A]): Reader[A] = Kleisli(f)

  def pure[A](a: A) = Reader(_ => valid(a))

}
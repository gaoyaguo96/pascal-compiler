package com.jourei.compiler.data
import cats.syntax.option.*

opaque type LStack[A] = List[A]

object LStack {
  def empty[A]: LStack[A] = List.empty

  def apply[A](as: A*): LStack[A] = List(as*)

  extension [A](stack: LStack[A]) {
    def popOption(): Option[LStack[A]] =
      if stack.isEmpty then Option.empty else stack.tail.some

    def level: Option[Int] =
      val length = stack.length
      if length < 0 then Option.empty
      else (length - 1).some

    def pop(): LStack[A] = stack.tail

    def push(a: A): LStack[A] = a :: stack

    def isEmpty: Boolean = stack.isEmpty

    def isNotEmpty: Boolean = !isEmpty

    def headOption: Option[A] = stack.headOption

    def head: A = stack.head

    def map[B](f: A => B): LStack[B] = stack.map(f)

    def foldl[B](z: B)(f: (B, A) => B): B = stack.foldLeft(z)(f)
  }
}

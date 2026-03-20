package u03

import scala.annotation.tailrec

object Streams extends App :

  import Sequences.*

  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def toList[A](stream: Stream[A]): Sequence[A] = stream match
      case Cons(h, t) => Sequence.Cons(h(), toList(t()))
      case _ => Sequence.Nil()

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _ => Empty()

    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if (pred(head())) => cons(head(), filter(tail())(pred))
      case Cons(head, tail) => filter(tail())(pred)
      case _ => Empty()

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

    def takeWhile[A](s: Stream[A])(p: A => Boolean): Stream[A] = s match
      case Cons(h, t) => p(h()) match
        case true => cons(h(), takeWhile(t())(p))
        case _ => Empty()
      case _ => Empty()

    def fill[A](n: Int)(k: A): Stream[A] = n match
      case x if x > 0 => cons(k, fill(n - 1)(k))
      case _ => empty()

    def fibonacci(n: Int): Stream[Int] =
      def helper(prev1: Int, prev2: Int, accumulator: Int): Stream[Int] =
        accumulator match
          case x if x == n => empty()
          case _ => cons(prev2, helper(prev2, prev1 + prev2, accumulator + 1))
      helper(1, 0, 0)

    def interleave[A](s1: Stream[A], s2: Stream[A]): Stream[A] = s1 match
      case Cons(h, t) => cons(h(), interleave(s2, t()))
      case _ => s2 match
        case Cons(h, t) => cons(h(), interleave(t(), Empty()))
        case _ => Empty()

    def fromList[A](list: List[A]): Stream[A] =
      def helper(index: Int): Stream[A] = index match
        case x if x >= list.size => Empty()
        case _ => cons(list(index), helper(index + 1))
      helper(0)

    def cycle[A](lst: Sequence[A]): Stream[A] =
      def helper(s: Sequence[A]): Stream[A] = s match
        case Sequence.Cons(h, t) => cons(h, helper(t))
        case _ => helper(lst)
      helper(lst)

  end Stream

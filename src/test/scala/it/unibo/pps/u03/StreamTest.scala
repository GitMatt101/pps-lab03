package it.unibo.pps.u03

import org.junit.Assert.assertEquals
import org.junit.Test
import u03.Sequences.Sequence
import u03.Streams.*
import u03.Streams.Stream.*
import u03.Sequences.Sequence.*

class StreamTest:

  @Test def testFromList(): Unit =
    val stream = Stream.iterate(0)(_ + 1)
    val sequence = toList(Stream.take(stream)(5))
    assertEquals(sequence, toList(fromList(List(0, 1, 2, 3, 4))))
  
  @Test def testTakeWhile(): Unit =
    val stream = Stream.iterate(0)(_ + 1)
    val sequence = toList(Stream.take(stream)(5))
    assertEquals(sequence, toList(takeWhile(stream)(_ < 5)))

  @Test def testFill(): Unit =
    val stream = Stream.fill(5)("a")
    assertEquals(toList(stream), Cons("a", Cons("a", Cons("a", Cons("a", Cons("a", Nil()))))))

  @Test def testFibonacci(): Unit =
    val stream = Stream.fibonacci(6)
    assertEquals(toList(stream), Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Nil())))))))

  @Test def testInterleave(): Unit =
    val s1 = fromList(List(1, 3, 5))
    val s2 = fromList(List(2, 4, 6, 8, 10))
    assertEquals(toList(interleave(s1, s2)), toList(fromList(List(1, 2, 3, 4, 5, 6, 8, 10))))

  @Test def testCycle(): Unit =
    val list = Sequence.Cons("a", Sequence.Cons("b", Sequence.Cons("c", Sequence.Nil())))
    assertEquals(toList(fromList(List("a", "b", "c", "a", "b"))), toList(take(cycle(list))(5)))